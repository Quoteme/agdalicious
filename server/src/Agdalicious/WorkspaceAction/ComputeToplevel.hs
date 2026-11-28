-- | Evaluating an expression in the scope of a document, outside any hole:
-- Agda's @Cmd_compute_toplevel@, what the Emacs mode's @C-c C-n@ runs
-- outside a goal. Each way of evaluating (the @ComputeMode@s) is a command
-- of its own, as the goal commands' are ("Agdalicious.CodeAction.GoalCommand").
module Agdalicious.WorkspaceAction.ComputeToplevel
  ( Compute (..)
  , computeCommandName
  , computes
  , computeLine
  , computeResult
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _String)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | A way to evaluate: its title and Agda's @ComputeMode@ for it.
data Compute = Compute {computeTitle :: Text, computeMode :: Text}
  deriving (Show)

-- | The workspace command that evaluates. Its arguments: the document, the
-- @ComputeMode@, and the expression, which an editor that is not given one
-- asks for (see @nvim/lua/agdalicious/lsp.lua@).
computeCommandName :: Text
computeCommandName = "agdalicious.compute"

computes :: [Compute]
computes =
  [ Compute "Evaluate to normal form" "DefaultCompute"
  , Compute "Evaluate to head normal form" "HeadCompute"
  , Compute "Evaluate, ignoring abstract" "IgnoreAbstract"
  , Compute "Evaluate and show (with its Show instance)" "UseShowInstance"
  ]

-- | The line that evaluates an expression in a @ComputeMode@.
computeLine :: Text -> Text -> String
computeLine mode expr = "Cmd_compute_toplevel " <> T.unpack mode <> " " <> show (T.unpack expr)

-- | What Agda's responses say: the normal form, or the error ('Left').
computeResult :: [Aeson.Value] -> Either Text Text
computeResult rs = maybe (Left "Agda gave no result") id (listToMaybe (mapMaybe result rs))
  where
    result r = do
      info <- r ^? key "info"
      case info ^? key "kind" . _String of
        Just "NormalForm" -> Right <$> info ^? key "expr" . _String
        Just "Error" -> Left <$> (info ^? key "error" . key "message" . _String <|> info ^? key "message" . _String)
        _ -> Nothing
