-- | The code actions: fixes for what Agda reports, each kind in a module of
-- its own under @Agdalicious.CodeAction@, re-exported here.
module Agdalicious.CodeAction
  ( module Agdalicious.CodeAction.Fix
  , module Agdalicious.CodeAction.ImportSuggestion
  ) where

import Agdalicious.CodeAction.Fix
import Agdalicious.CodeAction.ImportSuggestion
