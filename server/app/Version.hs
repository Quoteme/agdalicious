{-# LANGUAGE TemplateHaskell #-}

-- | The commit the server was built from, fixed at compile time: the
-- @AGDALICIOUS_COMMIT@ environment variable if it is set (as the GitHub
-- Actions workflow and the flake do), else what @git@ says about the
-- checkout (with @-dirty@ for uncommitted changes), else @unknown@.
module Version (commit) where

import Language.Haskell.TH (litE, stringL)
import Version.Commit (found)

commit :: String
commit = $(litE . stringL =<< found)
