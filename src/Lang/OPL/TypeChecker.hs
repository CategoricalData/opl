module Lang.OPL.TypeChecker where

import Prelude()
import FP
import qualified FP.Pretty as P
import System.Console.ANSI
import Text.Parsec (SourcePos)
import Lang.OPL.Syntax
import Data.Lens.Template
import Lang.OPL.Annotated
import qualified Data.List as List

-- remember to check surjectivity (failure means 'unsafe'), possibly making
-- it optional because the math still works out without the check.

type Context = [(SourcePos, String)]
data Message = Message
  { messagePhase :: String
  , messageContext :: Context
  , messageTitle :: String
  , messageDescription :: Maybe String
  } deriving (Eq, Ord, Show)

instance Pretty Message where
  pretty m = do
    P.text "error during phase: " 
    P.localConsole (mappend $ setConsoleColor Dull Magenta) $ 
      P.string (messagePhase m)
    P.hardLine
    let reason = do
          P.localConsole (mappend $ setConsoleColor Dull Red) $
            P.string $ messageTitle m
          case messageDescription m of
            Nothing -> return ()
            Just d -> do
              P.hardLine
              P.string d
    (\ f -> List.foldl' f reason $ messageContext m) $ \ i (l, s) -> do
      P.text "in " 
      P.localConsole (mappend $ setConsoleColor Dull Cyan) $
        P.string s 
      P.localConsole (mappend $ setConsoleColor Dull Yellow) $ do
        P.text " ["
        P.string $ show l
        P.text "]"
      P.hardLine
      P.space 2 >> P.align i

data CheckEnv = CheckEnv
  { _phase :: String
  , _context :: Context
  } deriving (Eq, Ord, Show)
makeLens ''CheckEnv

checkEnv0 :: CheckEnv
checkEnv0 = CheckEnv
  { _phase = "<main>"
  , _context = []
  }

data CheckState = CheckState
  { _regBoxes :: [(AName, Box)]
  , _regWiringDiagrams :: [(AName, WiringDiagram)]
  , _regWiringCompositions :: [(AName, WiringComposition)]
  , _warnings :: [Message]
  } deriving (Eq, Ord, Show)
makeLens ''CheckState

checkState0 :: CheckState
checkState0 = CheckState
  { _regBoxes = []
  , _regWiringDiagrams = []
  , _regWiringCompositions = []
  , _warnings = []
  }

type Check m =
  ( MonadError Message m
  , MonadReaderView CheckEnv m
  , MonadStateView CheckState m
  )

isCheck :: (Check m) => m a
isCheck = error "why would you evaluate this?!?!"

-------------------- generic helpers --------------------

inContext :: (Check m) => String -> AName -> m a -> m a
inContext s (Annotated l n) = 
  localViewMod context $ (:) (l, s ++ " " ++ n)

inPathContext :: (Check m) => String -> APath -> m a -> m a
inPathContext s (Annotated l p) =
  localViewMod context $ (:) (l, s ++ " " ++ prettyPath p)

withPhase :: (Check m) => String -> m () -> m ()
withPhase = localViewSet phase

checkError :: (Check m) => String -> Maybe String -> m a
checkError name description = do
  p <- askView phase
  c <- askView context
  throwError $ Message p c name description

checkWarning :: (Check m) => String -> Maybe String -> m ()
checkWarning name description = do
  p <- askView phase
  c <- askView context
  modifyView warnings $ (:) $ Message p c name description

allUnique :: (Eq a) => [a] -> Bool
allUnique xs = length (List.nub xs) == length xs

-- TODO: have this identify which name is the duplicate (using context)
checkNoDups :: (Check m) => [AName] -> m ()
checkNoDups ans = do
  let ns = map stripAnnotation ans
  when (not $ allUnique ns) $
    checkError "duplicate" $ Just $ "found in " ++ show ns

setEquiv :: (Ord a) => [a] -> [a] -> Bool
setEquiv xs ys = List.sort xs == List.sort ys

checkSetEquiv :: (Check m) => [AName] -> [AName] -> m ()
checkSetEquiv ans1 ans2 = do
  let ns1 = map stripAnnotation ans1
      ns2 = map stripAnnotation ans2
  when (not $ setEquiv ns1 ns2) $
    checkError "not equivalent" $ Just $ "two sets should be equivalent " ++ show ns1 ++ show  ns2

checkEqual :: (Check m, Show a, Eq a) => a -> a -> m ()
checkEqual x y =
  when (not $ x == y) $
    checkError "not equal" $ Just $ "two objects should be equal " ++ show x ++ show y

---------- registering checked defs ----------

registerBox :: (Check m) => AName -> Box -> m ()
registerBox n b = modifyView regBoxes $ (:) (n, b)

registerWiringDiagram :: (Check m) => AName -> WiringDiagram -> m ()
registerWiringDiagram n wd = modifyView regWiringDiagrams $ (:) (n, wd)

registerWiringComposition :: (Check m) => AName -> WiringComposition -> m ()
registerWiringComposition n wc = modifyView regWiringCompositions $ (:) (n, wc)

--------------------  type checker -------------------- 

check :: (Check m) => [Def] -> m ()
check ds = 
  withPhase "typechecking" $
    mapM_ checkDef ds

checkDef :: (Check m) => Def -> m ()
checkDef (BoxDef aname box) = do
  inContext "box" aname $ checkBox box
  registerBox aname box
checkDef (WiringDiagramDef aname wd) = do
  inContext "wiring diagram" aname $ checkWiringDiagram wd
  registerWiringDiagram aname wd
checkDef (WiringCompositionDef aname wc) = do
  inContext "wiring composition" aname $ checkWiringComposition wc
  registerWiringComposition aname wc

---------- boxes ----------

validTypes :: [Name]
validTypes =
  [ "int"
  , "float"
  , "bool"
  ]

checkValidType :: (Check m) => Type -> m ()
checkValidType t@(Annotated l n) = do
  inContext "type" t $
    when (not $ n `elem` validTypes) $
      checkError "invalid" $ Just $ "must be an element of " ++ show validTypes

checkBox :: (Check m) => Box -> m ()
checkBox (Box inputs outputs) = do
  let all = inputs ++ outputs
  checkNoDups $ map binderName inputs
  checkNoDups $ map binderName outputs
  forM_ all $ \ (Binder aname ttype) ->
    inContext "plug" aname $
      checkValidType ttype

---------- wiring diagrams ----------

data WiringTag = Internal | External
  deriving (Eq, Ord, Show)

-- a valid source for a path is either an output of an internal box or an
-- input of an external box
checkValidSource :: (Check m) => [(AName, [(AName, Type)])] -> APath -> m Type
checkValidSource validSources apath = do
  inPathContext "path" apath $ do
    -- check that path has exactly two levels
    (root, nodeName) <- case stripAnnotation apath of
      root :.: SingletonPath nodeName -> return (root, nodeName)
      _ -> checkError "invalid" $ Just $ "must be a two-level path" 
    -- check that the root exists in existing definitions of wirings
    nodes <- case lookup root $ map (first stripAnnotation) validSources of
      Nothing -> checkError "invalid" $ Just $ "root must exist as an internal or external box"
      Just w -> return w
    -- return the type of the node, checking first that it exists
    case lookup nodeName $ map (first stripAnnotation) nodes of
      Nothing -> do
        let d = concat
              [ "name must exist as either "
              , "an output of an internal box "
              , "or an input of the external box"
              ]
        checkError "invalid" $ Just d 
      Just t -> return t

checkWiringDiagram :: (Check m) => WiringDiagram -> m ()
checkWiringDiagram (WiringDiagram internalWirings externalWiring) = do
  let names = wiringName externalWiring : map wiringName internalWirings
      internalBinders = map wiringBinder internalWirings
      internalPlugs = map wiringPlugs internalWirings
      Wiring externalBinder externalPlug = externalWiring
  -- make sure there are no duplicates in binding names
  checkNoDups names
  -- lookup box definitions
  internalBoxBinders <- mapM getBox internalBinders
  externalBoxBinder <- getBox externalBinder
  let all = map reassoc $
        (External, externalBoxBinder, externalPlug) 
        : zip3 (repeat Internal) internalBoxBinders internalPlugs
      reassoc (t, (n, b), p) = (n, (t, b, p))
      -- valid source nodes are either an output (if internal) or input (if
      -- external) of the box.
      validSources = (\ f -> List.foldl' f [] all) $ \ i (name, (tag, box, _)) ->
        let nodes = case tag of
              Internal -> boxOutputs box
              External -> boxInputs box
        in (name, map binderToTuple nodes) : i
      validSourcePaths = List.sort $ List.nub $ do
        (name, nodes) <- validSources
        flip map nodes $ \ (n, _) -> 
          stripAnnotation name :.: SingletonPath (stripAnnotation n)
      mappedSourcePaths = 
        List.sort $ List.nub $ 
        (\ f -> List.foldl' f [] all) $ \ i (_, (_, _, plugs)) ->
          map (stripAnnotation . plugPath) plugs ++ i
  -- check internal boxes
  forM_ all $ \ (aname, (tag, box, plugs)) -> do
    inContext "wiring" aname $ do
      -- the nodes that plugs must map to are different for internal and
      -- external boxes
      let nodes = case tag of
            Internal -> boxInputs box
            External -> boxOutputs box
      -- make sure plugs correspond to nodes
      checkSetEquiv (map plugName plugs) (map binderName nodes)
      -- check individual plug integrity
      forM plugs $ \ (Plug name path) -> do
        -- get the type for the input node
        let typeBox = fromJust $ lookupBinder name nodes
        -- validate the path and return the type of the source
        typeSource <- checkValidSource validSources path
        -- check that the input node type matches the source type
        checkEqual typeBox typeSource
  -- check surjectivity
  let surjective = validSourcePaths == mappedSourcePaths
  when (not surjective) $
    checkError "not surjective" $ Just $ concat
      [ "wirings must be surjective: no mapping for "
      , List.intercalate ", " $ map prettyPath $ validSourcePaths List.\\ mappedSourcePaths
      ]

getBox :: (Check m) => Binder -> m (AName, Box)
getBox (Binder n t) = do
  bs <- getView regBoxes
  case lookup t bs of
    Nothing ->
      inContext "lookup" t $
        checkError "not defined" Nothing
    Just b -> return (n, b)

---------- wiring compositions ----------

checkWiringComposition :: (Check m) => WiringComposition -> m ()
checkWiringComposition (WiringComposition internalWds externalWd) = return ()
