-- | Writing @?@-holes as @{! !}@, which has room to type in, as the Emacs
-- mode does (and cornelis' @:CornelisQuestionToMeta@). Agda has no command
-- for this: where the holes are comes from its interaction points.
module Agdalicious.CodeAction.QuestionToMeta (questionToMeta) where

import Agdalicious.CodeAction.Fix (Fix (..))
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Range (..))

-- | Given the ranges of the document's @?@-holes and of the cursor (or
-- selection): a fix for the holes there, and one for all of them.
questionToMeta :: [Range] -> Range -> [Fix]
questionToMeta holes cursor =
  [Fix "Expand ? to {! !}" (expand here) | not (null here), length holes > 1]
    <> [Fix (if length holes == 1 then "Expand ? to {! !}" else "Expand all " <> count <> " ?-holes to {! !}") (expand holes) | not (null holes)]
  where
    here = filter (overlaps cursor) holes
    count = T.pack (show (length holes))
    expand = map (,"{! !}")
    overlaps (Range s e) (Range s' e') = s <= e' && s' <= e
