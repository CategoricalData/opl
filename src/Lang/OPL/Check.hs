module Lang.OPL.Check where

import Prelude()
import FP
import Lang.OPL.Syntax
import Data.Lens.Template
import Lang.OPL.Annotated
import Lang.OPL.Message
import qualified Data.List as List

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
  , _regWiringDiagrams :: [(AName, (WiringDiagram, WiringDiagramType))]
  , _regWiringCompositions :: [(AName, (WiringComposition, WiringDiagramType))]
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
    checkError "duplicate" $ Just $ "found in " ++ List.intercalate " " ns

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

boxEquiv :: Box -> Box -> Bool
boxEquiv (Box inx outx) (Box iny outy) =
  (binderListSort inx == binderListSort iny) 
  && (binderListSort outx == binderListSort outy)
  where
    binderListSort = List.sortBy (compare `on` binderName)

---------- registering checked defs ----------

registerBox :: (Check m) => AName -> Box -> m ()
registerBox n b = modifyView regBoxes $ (:) (n, b)

registerWiringDiagram :: (Check m) => AName -> WiringDiagram -> WiringDiagramType -> m ()
registerWiringDiagram n wd wdt = modifyView regWiringDiagrams $ (:) (n, (wd, wdt))

registerWiringComposition :: (Check m) => AName -> WiringComposition -> WiringDiagramType -> m ()
registerWiringComposition n wc wdt = modifyView regWiringCompositions $ (:) (n, (wc, wdt))

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
  wdt <- inContext "wiring diagram" aname $ checkWiringDiagram wd
  registerWiringDiagram aname wd wdt
checkDef (WiringCompositionDef aname wc) = do
  wdt <- inContext "wiring composition" aname $ checkWiringComposition wc
  registerWiringComposition aname wc wdt

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

getBoxBinder :: (Check m) => Binder -> m BoxBinder
getBoxBinder (Binder n t) = do
  bs <- getView regBoxes
  case lookup t bs of
    Nothing ->
      inContext "lookup" t $
        checkError "not defined" Nothing
    Just b -> return $ BoxBinder n b

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

checkWiringDiagram :: (Check m) => WiringDiagram -> m WiringDiagramType
checkWiringDiagram (WiringDiagram internalWirings externalWiring) = do
  let names = wiringName externalWiring : map wiringName internalWirings
      internalBinders = map wiringBinder internalWirings
      internalPlugs = map wiringPlugs internalWirings
      Wiring externalBinder externalPlug = externalWiring
  -- make sure there are no duplicates in binding names
  checkNoDups names
  -- lookup box definitions
  internalBoxBinders <- mapM getBoxBinder internalBinders
  externalBoxBinder <- getBoxBinder externalBinder
  let all = map reassoc $
        (External, externalBoxBinder, externalPlug) 
        : zip3 (repeat Internal) internalBoxBinders internalPlugs
      reassoc (t, (BoxBinder n b), p) = (n, (t, b, p))
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
  return $ WiringDiagramType internalBoxBinders $ boxBinderBox externalBoxBinder

lookupWiringDiagram :: (Check m) => AName -> m (WiringDiagram, WiringDiagramType)
lookupWiringDiagram n = do
  wds <- getView regWiringDiagrams
  case lookup n wds of
    Nothing ->
      inContext "lookup" n $
        checkError "not defined" Nothing
    Just wd -> return wd

---------- wiring compositions ----------

renameBoxBinder :: [Mapping] -> BoxBinder -> BoxBinder
renameBoxBinder renamings (BoxBinder name box) =
  let newName = fromJust $ lookupMapping name renamings
  in BoxBinder newName box

checkWiringComposition :: (Check m) => WiringComposition -> m WiringDiagramType
checkWiringComposition (WiringComposition externalWdName internalWdFillings) = do
  -- lookup external wiring diagram definition
  (_, externalWdType) <- lookupWiringDiagram externalWdName
  -- the boxBinders that need filling
  let toFill = wiringDiagramTypeInternalBoxBinders externalWdType
      toFillNames = List.sort (map boxBinderName toFill)
  -- the names that are filled
  let filledNames = List.sort $ map (mappingKey . fillingMapping) internalWdFillings
  -- check that the specified fillings match exactly
  let filledNamesMatches = toFillNames == filledNames
  when (not filledNamesMatches) $
    checkError "incorrect fills" $ Just $ concat
      [ "must provide fillings for "
      , "wiring diagram inputs exactly: "
      , "expected " ++ List.intercalate " " (map stripAnnotation toFillNames) ++ ", "
      , "recieved " ++ List.intercalate " " (map stripAnnotation filledNames)
      ]
  -- check fillings
  internalBoxBinders <- 
    forM internalWdFillings $ \ (Filling (Mapping holeName holeWdName) exports) -> do
      inContext "filling for" holeName $ do
        -- lookup wiring diagram of filling
        (wd, wdt) <- lookupWiringDiagram holeWdName
        -- lookup box type of hole
        let hole = fromJust $ lookupBoxBinder holeName toFill
        -- check that the external box of the wiring diagram to fill matches
        -- the hole
        let boxMatches = boxEquiv (wiringDiagramTypeExternalBox wdt) hole
        when (not boxMatches) $
          checkError "incorrect fill type" Nothing {- $ Just $ concat
            [ "hole expects box " ++ show hole ++ ", "
            , "received " ++ show (wiringDiagramTypeExternalBox wdt)
            ] -}
        -- check exports sources
        let renamed = List.sort $ map mappingKey exports
            wdNames = List.sort $ map boxBinderName $ wiringDiagramTypeInternalBoxBinders wdt
            renamedMatches = renamed == wdNames
        when (not renamedMatches) $
          checkError "incorrect renaming" $ Just $ concat
            [ "must provide renamings for "
            , "wiring diagram inputs exactly: "
            , "expected " ++ List.intercalate " " (map stripAnnotation wdNames) ++ ", "
            , "received " ++ List.intercalate " " (map stripAnnotation renamed)
            ]
        return $ map (renameBoxBinder exports) $ wiringDiagramTypeInternalBoxBinders wdt
  -- check that all export targets are unique
  let allExportTargets = map mappingValue $ concat $ map fillingExports internalWdFillings
  when (not $ allUnique allExportTargets) $
    checkError "duplicate export" $ Just $ 
      "found in " ++ List.intercalate " " (map stripAnnotation allExportTargets)
  return $ WiringDiagramType (concat internalBoxBinders) (wiringDiagramTypeExternalBox externalWdType)
