module Lang.OPL.Check where

import Prelude()
import FP

import Data.Lens.Template
import Data.Map (Map)
import Data.Set (Set)
import Lang.OPL.Common
import Lang.OPL.Annotated
import Lang.OPL.Message
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FP.Pretty as P
import qualified Lang.OPL.Semantics as Sem
import qualified Lang.OPL.Syntax as Syn

data CheckEnv = CheckEnv
  { _phaseL :: String
  , _contextL :: Context
  } deriving (Eq, Ord, Show)
makeLens ''CheckEnv

checkEnv0 :: CheckEnv
checkEnv0 = CheckEnv
  { _phaseL = "<main>"
  , _contextL = []
  }

data Registered a t = 
    Declared t
  | Defined a t
  deriving (Eq, Ord, Show)
data CheckState = CheckState
  { _boxesL :: Map AName (Registered Sem.Box ())
  , _wiringExpsL :: Map AName (Registered Sem.WiringExp Sem.WiringType)
  , _warningsL :: [Message]
  } deriving (Eq, Ord, Show)
makeLens ''CheckState

checkState0 :: CheckState
checkState0 = CheckState
  { _boxesL = Map.empty
  , _wiringExpsL = Map.empty
  , _warningsL = []
  }

type Check m =
  ( MonadError Message m
  , MonadReaderView CheckEnv m
  , MonadStateView CheckState m
  )

isCheck :: (Check m) => m a
isCheck = error "why would you evaluate this?!?!"

-------------------- generic helpers --------------------

simpleContext :: (Check m) => String -> m a -> m a
simpleContext s =
  localViewMod contextL $ (:) (Nothing, PrettyString $ P.string s)

inContext :: (Check m) => String -> AName -> m a -> m a
inContext s (Annotated l n) = 
  localViewMod contextL $ (:) (Just l, PrettyString $ P.hsep [P.string s, pretty n])

inPathContext :: (Check m) => String -> APath -> m a -> m a
inPathContext s (Annotated l p) =
  localViewMod contextL $ (:) (Just l, PrettyString $ P.hsep [P.string s, pretty p])

withPhase :: (Check m) => String -> m () -> m ()
withPhase = localViewSet phaseL

checkError :: (Check m) => String -> Maybe PrettyString -> m a
checkError name description = do
  p <- askView phaseL
  c <- askView contextL
  throwError $ Message (PrettyString $ P.string p) c (PrettyString $ P.string name) description

checkWarning :: (Check m) => String -> Maybe PrettyString -> m ()
checkWarning name description = do
  p <- askView phaseL
  c <- askView contextL
  modifyView warningsL $ (:) $ Message (PrettyString $ P.string p) c (PrettyString $ P.string name) description

checkNoDup :: (Check m, Ord a, Pretty a) => [a] -> m (Set a)
checkNoDup xs = do
  let xset = Set.fromList xs
  when (List.length xs /= Set.size xset) $
    checkError "duplicate" $ Just $ PrettyString $ P.hsep
      [ P.string "found in"
      , pretty xs
      ]
  return xset

checkExactZip :: (Check m, Pretty a, Pretty b) => [a] -> [b] -> m [(a, b)]
checkExactZip xs ys = do
  when (List.length xs /= List.length ys) $
    checkError "lists not of same length" $ Just $ PrettyString $ P.vsep
      [ pretty xs
      , P.string "<>"
      , pretty ys
      ]
  return $ zip xs ys

checkEqual :: (Check m, Pretty a, Eq a) => a -> a -> m ()
checkEqual x y =
  when (not $ x == y) $
    checkError "not equal" $ Just $ PrettyString $ P.vsep
      [ pretty x
      , P.string "<>"
      , pretty y
      ]

checkNotEqual :: (Check m, Pretty a, Eq a) => a -> a -> m ()
checkNotEqual x y =
  when (x == y) $
    checkError "equal" $ Just $ PrettyString $ P.vsep
      [ pretty x
      , P.string "=="
      , pretty y
      ]

---------- registering checked defs ----------

declareBox :: (Check m) => AName -> m ()
declareBox n = do
  bs <- getView boxesL
  let action = putView boxesL $ Map.insert n (Declared ()) bs
  case Map.lookup n bs of
    Nothing -> action
    Just (Declared ()) -> checkError "repeat declaration" Nothing
    Just (Defined _ _) -> checkError "already defined" Nothing

defineBox :: (Check m) => AName -> Sem.Box -> m ()
defineBox n b = do
  bs <- getView boxesL
  let action = putView boxesL $ Map.insert n (Defined b ()) bs
  case Map.lookup n bs of
    Nothing -> action
    Just (Declared ()) -> action
    Just (Defined _ _) -> checkError "repeat definition" Nothing

lookupBox :: (Check m) => AName -> m Sem.Box
lookupBox n = do
  e <- getView boxesL
  case Map.lookup n e of
    Nothing -> checkError "not defined" Nothing
    Just (Declared _) -> checkError "declared but not defined" Nothing
    Just (Defined b ()) -> return b

declareWiring :: (Check m) => AName -> Sem.WiringType -> m ()
declareWiring n wdt = do
  ws <- getView wiringExpsL
  let action = putView wiringExpsL $ Map.insert n (Declared wdt) ws
  case Map.lookup n ws of
    Nothing -> action
    Just (Declared _) -> checkError "repeat declaration" Nothing
    Just (Defined _ _) -> checkError "already defined" Nothing

registerWiring :: (Check m) => AName -> Sem.WiringExp -> Sem.WiringType -> m ()
registerWiring n wd wdt = do
  ws <- getView wiringExpsL
  let action = putView wiringExpsL $ Map.insert n (Defined wd wdt) ws
  case Map.lookup n ws of
    Nothing -> action
    Just (Declared wdt') -> do
      checkEqual wdt wdt'
      action
    Just (Defined _ _) -> checkError "repeat definition" Nothing

lookupWiringExp :: (Check m) => AName -> m (Sem.WiringExp, Sem.WiringType)
lookupWiringExp n = do
  ws <- getView wiringExpsL
  case Map.lookup n ws of
    Nothing -> checkError "not defined" Nothing
    Just (Declared _) -> checkError "declared but not defined" Nothing
    Just (Defined we wt) -> return (we, wt)

--------------------  type checker -------------------- 

check :: (Check m) => Syn.TLModule -> m ()
check (Syn.TLModule m) = withPhase "typechecking" $ checkModule m

checkModule :: (Check m) => Syn.Module -> m ()
checkModule (Syn.Module _ _ _ ss) = mapM_ checkStatement ss

checkStatement :: (Check m) => Syn.Statement -> m ()
checkStatement (Syn.DeclStatement decl) = checkDecl decl
checkStatement (Syn.DefStatement def) = checkDef def

checkDecl :: (Check m) => Syn.Decl -> m ()
checkDecl (Syn.AlgebraDecl n) = inContext "algebra" n $ checkWarning "not supported" Nothing
checkDecl (Syn.ModuleDecl n) = inContext "module" n $ checkWarning "not supported" Nothing
checkDecl (Syn.BoxDecl n) = inContext "box" n $ declareBox n
checkDecl (Syn.WiringDecl n wte) = inContext "define" n $ do
  wt <- checkWiringType wte
  declareWiring n wt

checkDef :: (Check m) => Syn.Def -> m ()
checkDef (Syn.AlgebraDef n _) = inContext "algebra" n $ checkWarning "not supported" Nothing
checkDef (Syn.ModuleDef n _) = inContext "module" n $ checkWarning "not supported" Nothing
checkDef (Syn.BoxDef n be) = inContext "box" n $ do
  b <- checkBox be
  defineBox n b
checkDef (Syn.WiringDef n we) = inContext "define" n $ do
  (w, wt) <- checkWiringExp we
  registerWiring n w wt

---------- boxes ----------

validPlugTypes :: [Name]
validPlugTypes = map Name
  [ "int"
  , "float"
  , "bool"
  ]

checkPlugType :: (Check m) => PlugType -> m ()
checkPlugType t = do
  inContext "plug type" (getPlugType t) $
    when (not $ stripAnnotation (getPlugType t) `elem` validPlugTypes) $
      checkError "invalid" $ Just $ PrettyString $ P.hsep
        [ P.string "must be an element of"
        , pretty validPlugTypes
        ]

checkBox :: (Check m) => Syn.Box -> m Sem.Box
checkBox (Syn.VarBox n) = inContext "var" n $ lookupBox n
checkBox (Syn.ArrowBox ab) = checkBoxArrow ab

checkBoxArrow :: (Check m) => BoxArrow -> m Sem.Box
checkBoxArrow (BoxArrow inputs outputs) = do
  forM_ (inputs ++ outputs) checkPlugType
  return $ Sem.Box inputs outputs

---------- wiring diagrams ----------

checkWiringArrow :: (Check m) => Syn.WiringArrow -> m Sem.WiringArrow
checkWiringArrow (Syn.WiringArrow ins out) = do
  ins' <- mapM checkBox ins
  out' <- checkBox out
  return $ Sem.WiringArrow ins' out'

checkWiringType :: (Check m) => Syn.WiringType -> m Sem.WiringType
checkWiringType (Syn.VarWiringType _) = checkError "wiring type variables not supported" Nothing
checkWiringType (Syn.LiftWiringType n wt) = liftM (Sem.LiftWiringType n) $ checkWiringType wt
checkWiringType (Syn.BoxWiringType b) = liftM Sem.BoxWiringType $ checkBox b
checkWiringType (Syn.ArrowWiringType a) = liftM Sem.ArrowWiringType $ checkWiringArrow a

data Tag = Internal | External
  deriving (Eq, Ord, Show)
data BoundBox = BoundBox
  { boundBoxInputs :: Map Name PlugType
  , boundBoxOutputs :: Map Name PlugType
  } deriving (Eq, Ord, Show)

-- a valid source for a path is either an output of an internal box or an
-- input of an external box
checkValidSource :: (Check m) => Map Name (Map Name PlugType) -> APath -> m PlugType
checkValidSource validSources apath = do
  inPathContext "path" apath $ do
    -- check that path has exactly two levels
    (root, nodeName) <- case stripAnnotation apath of
      root :.: SingletonPath nodeName -> return (root, nodeName)
      _ -> checkError "invalid" $ Just $ PrettyString $ P.string "must be a two-level path" 
    -- check that the root exists in existing definitions of wirings
    nodes <- case Map.lookup root $ validSources of
      Nothing -> checkError "invalid" $ Just $ PrettyString $ P.string "root must exist as an internal or external box"
      Just w -> return w
    -- return the type of the node, checking first that it exists
    case Map.lookup nodeName $ nodes of
      Nothing -> do
        let d = concat
              [ "name must exist as either "
              , "an output of an internal box "
              , "or an input of the external box"
              ]
        checkError "invalid" $ Just $ PrettyString $ P.string d 
      Just t -> return t

checkWiringDiagram :: forall m. (Check m) => Syn.WiringDiagram -> m (Sem.WiringDiagram, Sem.WiringType)
checkWiringDiagram (Syn.WiringDiagram internalWirings externalWiring) = do
  -- make sure there are no duplicates in binding names
  let names = Syn.wiringName externalWiring : map Syn.wiringName internalWirings
  _ <- checkNoDup names
  let extractData :: Syn.Wiring -> (AName, ((Syn.Box, BoxBinder), [Plug]))
      extractData = Syn.wiringName &&& (Syn.wiringBox &&& Syn.wiringBoxBinder) &&& Syn.wiringPlugs
      internalData = map extractData internalWirings
      externalData = extractData externalWiring
      applyTemplate :: (Check m) => (Sem.Box, BoxBinder) -> m (Sem.Box, BoxBinder, BoundBox)
      applyTemplate (box@(Sem.Box ins outs), boxbin@(BoxBinder bins bouts)) = do
        _ <- checkNoDup bins
        _ <- checkNoDup bouts
        insMap <- checkExactZip (map stripAnnotation bins) ins
        outsMap <- checkExactZip (map stripAnnotation bouts) outs
        let bobo = BoundBox (Map.fromList insMap) (Map.fromList outsMap)
        return (box, boxbin, bobo)
      doBinder :: (AName, ((Syn.Box, BoxBinder), [Plug])) -> m (AName, Sem.Box, BoxBinder, BoundBox, [Plug])
      doBinder =
        return . (\ (n, ((b, bb, bobo), ps)) -> (n, b, bb, bobo, ps))
        <=< simpleContext "box template" . (secondM $ firstM $ applyTemplate)
        <=< simpleContext "box" . (secondM $ firstM $ firstM checkBox)
      packData :: (AName, Sem.Box, BoxBinder, BoundBox, [Plug]) -> Sem.Wiring
      packData (n, b, bb, _, ps) = Sem.Wiring n b bb ps
  internalBound <- mapM doBinder internalData
  externalBound <- doBinder externalData
  let allTagged :: [(Tag, (AName, Sem.Box, BoxBinder, BoundBox, [Plug]))]
      allTagged = (External,) externalBound : map (Internal,) internalBound
      validSources :: Map Name (Map Name PlugType)
      validSources = (\ c f -> List.foldl' f Map.empty c) allTagged $ flip $ \ (tag, (name, _, _, bobo, _)) ->
        Map.insert (stripAnnotation name) $ case tag of
          Internal -> boundBoxOutputs bobo
          External -> boundBoxInputs bobo
      validSourcePaths :: Set Path
      validSourcePaths = Set.fromList $ do
        (root, nodes) <- Map.toList validSources
        flip map (Map.keys nodes) $ \ n -> 
          root :.: SingletonPath n
      mappedSourcePaths :: Set Path
      mappedSourcePaths = (\ c f -> List.foldl' f Set.empty c) allTagged $ flip $ \ (_, (_, _, _, _, plugs)) ->
        Set.union $ Set.fromList $ map (stripAnnotation . plugPath) plugs
  -- check wirings
  forM_ allTagged $ \ (tag, (name, _, _, bobo, plugs)) -> do
    inContext "wiring" name $ do
      -- the nodes that plugs must map to are different for internal and
      -- external boxes
      let nodes = case tag of
            Internal -> boundBoxInputs bobo
            External -> boundBoxOutputs bobo
      -- make sure plugs correspond to nodes
      plugNames <- checkNoDup $ map (stripAnnotation . plugName) plugs
      checkEqual plugNames (Map.keysSet nodes)
      -- check individual plug integrity
      forM plugs $ \ (Plug name path) -> do
        -- get the type for the input node
        let plugType = fromJust $ Map.lookup (stripAnnotation name) nodes
        -- validate the path and return the type of the source
        sourcePlugType <- checkValidSource validSources path
        -- check that the input node type matches the source type
        checkEqual plugType sourcePlugType
  -- check surjectivity
  simpleContext "surjectivity check" $
    checkEqual validSourcePaths mappedSourcePaths
  -- check no straight wires
  simpleContext "no straight wires check" $ do
    let (n, _, _, _, explugs) = externalBound
    forM_ (map plugPath explugs) $ \ ppath -> do
      let root :.: SingletonPath _ = stripAnnotation ppath
      checkNotEqual (stripAnnotation n) root
  let internalResults = map packData internalBound
      externalResult = packData externalBound
  return 
    ( Sem.WiringDiagram internalResults externalResult
    , Sem.ArrowWiringType $ Sem.WiringArrow (map Sem.wiringBox internalResults) (Sem.wiringBox externalResult)
    )

checkLiftWiring :: (Check m) => AName -> Syn.WiringExp -> m (Sem.WiringExp, Sem.WiringType)
checkLiftWiring n e = do
  (we, wt) <- checkWiringExp e
  return (Sem.LiftWiringExp n we, Sem.LiftWiringType n wt)

permute :: (Check m, Pretty a) => Renaming -> [a] -> m [a]
permute (Renaming from to) xs = do
  fromSet <- checkNoDup from
  toSet <- checkNoDup to
  checkEqual fromSet toSet
  namedXs <- checkExactZip from xs
  return $ flip map to $ \ n ->
    fromJust $ List.lookup n namedXs

renameWiringArrow :: (Check m) => Renaming -> Sem.WiringArrow -> m Sem.WiringArrow
renameWiringArrow r (Sem.WiringArrow ins out) = do
  pins <- permute r ins
  return $ Sem.WiringArrow pins out

renameWiringType :: (Check m) => Renaming -> Sem.WiringType -> m Sem.WiringType
renameWiringType r (Sem.LiftWiringType n wt) = liftM (Sem.LiftWiringType n) $ renameWiringType r wt
renameWiringType r (Sem.BoxWiringType _) = checkError "cannot rename box type" Nothing
renameWiringType r (Sem.ArrowWiringType a) = liftM Sem.ArrowWiringType $ renameWiringArrow r a

checkRenamingWiring :: (Check m) => Renaming -> Syn.WiringExp -> m (Sem.WiringExp, Sem.WiringType)
checkRenamingWiring r e = do
  (we, wt) <- checkWiringExp e
  pwt <- renameWiringType r wt
  return (Sem.RenamingWiringExp r we, pwt)

checkApplyWiring :: (Check m) => Syn.WiringExp -> [Maybe Syn.WiringExp] -> m (Sem.WiringExp, Sem.WiringType)
checkApplyWiring wf waMs = do
  (wfv, wft) <- checkWiringExp wf
  wavtMs <- mapM (mapM checkWiringExp) waMs
  checkSemApplyWiring wfv wft wavtMs

checkSemApplyWiring :: forall m. (Check m) =>
  Sem.WiringExp -> Sem.WiringType -> [Maybe (Sem.WiringExp, Sem.WiringType)]
  -> m (Sem.WiringExp, Sem.WiringType)
checkSemApplyWiring wfv wft wavtMs =
  case wft of
    Sem.LiftWiringType n unWft -> do
      let Sem.LiftWiringExp _ unWfv = wfv
      unWavtMs <- mapM (mapM $ unliftArg n) wavtMs
      res <- checkSemApplyWiring unWfv unWft unWavtMs
      return $ liftResult n res
    Sem.BoxWiringType _ -> checkError "cannot apply a base box type" Nothing
    Sem.ArrowWiringType (Sem.WiringArrow ins out) -> do
      tapps <- 
        liftM (map $ second Syn.unUnderscored) 
        $ checkExactZip ins 
        $ map (Syn.Underscored . map snd) wavtMs
      let eapps = map (map fst) wavtMs
      tMs <- (\ c f -> foldr f (return []) c) tapps $ \ (int, tM) rest -> do
        let k f = liftM f rest
        case tM of
          Nothing -> k (int:)
          Just t -> case t of
            Sem.LiftWiringType _ _ -> checkError "lifted types must not appear inside complex types" Nothing
            Sem.BoxWiringType b -> do
              checkEqual b int
              k id
            Sem.ArrowWiringType (Sem.WiringArrow ins out) -> do
              checkEqual out int
              k (ins++)
      let finalType = case tMs of
            [] -> Sem.BoxWiringType out
            ins -> Sem.ArrowWiringType $ Sem.WiringArrow ins out
      return (Sem.ApplyWiringExp wfv eapps, finalType)
  where
    unliftArg :: AName -> (Sem.WiringExp, Sem.WiringType) -> m (Sem.WiringExp, Sem.WiringType)
    unliftArg n (Sem.LiftWiringExp en e, Sem.LiftWiringType tn t) = do
      checkEqual n en
      checkEqual n tn
      return $ (e, t)
    unliftArg _ _ = checkError "cannot apply lifted to unlifted values" Nothing
    liftResult :: AName -> (Sem.WiringExp, Sem.WiringType) -> (Sem.WiringExp, Sem.WiringType)
    liftResult n (e, t) = (Sem.LiftWiringExp n e, Sem.LiftWiringType n t)

checkWiringExp :: (Check m) => Syn.WiringExp -> m (Sem.WiringExp, Sem.WiringType)
checkWiringExp (Syn.VarWiringExp n) = inContext "var" n $ lookupWiringExp n
checkWiringExp (Syn.LiftWiringExp n we) = checkLiftWiring n we
checkWiringExp (Syn.DiagramWiringExp wd) = do
  (swd, wt) <- checkWiringDiagram wd
  return (Sem.DiagramWiringExp swd, wt)
checkWiringExp (Syn.RenamingWiringExp r we) = checkRenamingWiring r we
checkWiringExp (Syn.ApplyWiringExp e eMs) = checkApplyWiring e eMs
