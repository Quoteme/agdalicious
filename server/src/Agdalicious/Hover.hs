-- | Hover texts, as markdown, in five parts (each left out if empty):
--
-- 1. what it is: its type, as Agda code
-- 2. a short explanation
-- 3. an example, or its definition if that is short
-- 4. links to its documentation
-- 5. a long explanation, under a rule
module Agdalicious.Hover (Info (..), render) where

import Data.Text (Text)
import Data.Text qualified as T

data Info = Info
  { signature :: [Text]
  , summary :: [Text]
  , code :: [Text]
  , links :: [(Text, Text)]
  -- ^ (label, url)
  , details :: [Text]
  }

instance Semigroup Info where
  Info a b c d e <> Info a' b' c' d' e' = Info (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Monoid Info where
  mempty = Info [] [] [] [] []

render :: Info -> Text
render Info {signature, summary, code, links, details} =
  T.intercalate "\n\n" . filter (not . T.null) $
    [ agda signature
    , T.intercalate "\n\n" summary
    , agda code
    , T.intercalate " · " ["[" <> l <> "](" <> u <> ")" | (l, u) <- links]
    , if null details then "" else "---\n\n" <> T.intercalate "\n\n" details
    ]
  where
    agda [] = ""
    agda ls = "```agda\n" <> T.intercalate "\n" ls <> "\n```"
