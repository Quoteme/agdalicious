module Main (main) where

import Agdalicous.Server qualified as Server
import System.Exit (ExitCode (..), exitWith)

main :: IO ()
main = do
  code <- Server.run
  exitWith (if code == 0 then ExitSuccess else ExitFailure code)
