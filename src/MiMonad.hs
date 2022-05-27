module MiMonad where

import Control.Monad.Except (ExceptT, runExceptT, throwError)

data MiError = DeviceNotFound
  deriving (Show)

type MiMonad = ExceptT MiError IO

runMi :: MiMonad a -> IO (Either MiError a)
runMi = runExceptT

throw :: MiError -> MiMonad a
throw = throwError

displayError :: MiError -> IO ()
displayError = print
