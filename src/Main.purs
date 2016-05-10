module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Nothing to see here. Have a look in the test folder"
