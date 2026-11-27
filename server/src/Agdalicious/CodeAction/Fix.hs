-- | What a code action does, for all kinds of them ("Agdalicious.CodeAction").
module Agdalicious.CodeAction.Fix (Fix (..)) where

import Data.Text (Text)
import Language.LSP.Protocol.Types (Range)

-- | A fix: what it is called, and its edits of the document.
data Fix = Fix {title :: Text, edits :: [(Range, Text)]}
