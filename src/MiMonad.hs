module MiMonad where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT (runStateT))
import qualified Data.IntSet as Set

data MiError = DeviceNotFound | ReadEventError
  deriving (Show)

newtype MiState = MiState {activeKeys :: Set.IntSet}

type MiMonad = StateT MiState (ExceptT MiError IO)

runMi :: MiMonad a -> IO (Either MiError (a, MiState))
runMi = runExceptT . flip runStateT (MiState {activeKeys = Set.empty})

throw :: MiError -> MiMonad a
throw = throwError

displayError :: MiError -> IO ()
displayError = print
