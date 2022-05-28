module MiMonad where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT (runStateT))

data MiError = DeviceNotFound | ReadEventError
  deriving (Show)

newtype MiState = MiState {activeKeys :: [Int]}

type MiMonad = StateT MiState (ExceptT MiError IO)

runMi :: MiMonad a -> IO (Either MiError (a, MiState))
runMi = runExceptT . flip runStateT (MiState {activeKeys = []})

throw :: MiError -> MiMonad a
throw = throwError

displayError :: MiError -> IO ()
displayError = print
