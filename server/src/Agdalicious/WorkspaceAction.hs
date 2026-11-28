-- | The workspace actions: commands an editor runs on a document as a whole
-- (@workspace/executeCommand@), not on a goal or a diagnostic, each kind in
-- a module of its own under @Agdalicious.WorkspaceAction@, re-exported here.
module Agdalicious.WorkspaceAction
  ( module Agdalicious.WorkspaceAction.ComputeToplevel
  ) where

import Agdalicious.WorkspaceAction.ComputeToplevel
