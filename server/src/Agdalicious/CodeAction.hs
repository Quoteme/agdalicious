-- | The code actions: fixes for what Agda reports, each kind in a module of
-- its own under @Agdalicious.CodeAction@, re-exported here.
module Agdalicious.CodeAction
  ( module Agdalicious.CodeAction.Fix
  , module Agdalicious.CodeAction.ImportSuggestion
  , module Agdalicious.CodeAction.ImportModule
  , module Agdalicious.CodeAction.QuestionToMeta
  , module Agdalicious.CodeAction.GoalCommand
  , module Agdalicious.CodeAction.GoalResult
  ) where

import Agdalicious.CodeAction.Fix
import Agdalicious.CodeAction.GoalCommand
import Agdalicious.CodeAction.GoalResult
import Agdalicious.CodeAction.ImportModule
import Agdalicious.CodeAction.ImportSuggestion
import Agdalicious.CodeAction.QuestionToMeta
