module Main (main) where

import Agdalicious.Server qualified as Server
import Data.Version (showVersion)
import Options.Applicative
import Paths_agdalicious_lsp (version)
import System.Exit (ExitCode (..), exitWith)
import Version (commit)

main :: IO ()
main = do
  () <- execParser (info (options <**> helper) (fullDesc <> progDesc "Language server for Agda"))
  code <- Server.run
  exitWith (if code == 0 then ExitSuccess else ExitFailure code)

-- | The server only talks over stdio; @--stdio@, which some editors pass to
-- every language server, changes nothing.
options :: Parser ()
options =
  infoOption ("agdalicious-lsp " <> showVersion version <> " (commit " <> commit <> ")") (long "version" <> short 'v' <> help "Print the version and the commit it was built from")
    <*> (() <$ switch (long "stdio" <> internal))
