module Lang.OPL.CheckMonad where

import Prelude()
import FP
import Lang.OPL.Check
import System.Exit
import Lang.OPL.Message

newtype CheckMonadT m a = CheckMonadT
  { unCheckMonadT :: ReaderT CheckEnv (StateT CheckState (EitherT Message m)) a }
  deriving 
  ( Monad
  , MonadReader CheckEnv
  , MonadState CheckState
  , MonadError Message
  )
type instance (MEnv (CheckMonadT m)) = CheckEnv
type instance (MState (CheckMonadT m)) = CheckState

type CheckMonad = CheckMonadT Identity

-------------------- CheckMonadT --------------------

isCheck_CheckMonadT :: (Monad m) => CheckMonadT m a
isCheck_CheckMonadT = isCheck

runCheckMonadT :: CheckEnv -> CheckState -> CheckMonadT m a -> m (Either Message (a, CheckState))
runCheckMonadT e s aM =
  runEitherT
  $ flip runStateT s
  $ flip runReaderT e
  $ unCheckMonadT aM

-------------------- CheckMonad --------------------

runCheckMonad :: CheckEnv -> CheckState -> CheckMonad a -> Either Message (a, CheckState)
runCheckMonad = runIdentity ..: runCheckMonadT

execCheckMonad :: CheckEnv -> CheckState -> CheckMonad a -> Either Message CheckState
execCheckMonad = liftM snd ..: runCheckMonad

execCheckMonadIO :: CheckEnv -> CheckState -> CheckMonad a -> IO CheckState
execCheckMonadIO e s aM = case execCheckMonad e s aM of
  Left m -> do
    pprintLn m
    exitFailure
  Right a -> return a

execCheckMonadIO0 :: CheckMonad a -> IO CheckState
execCheckMonadIO0 = execCheckMonadIO checkEnv0 checkState0
