{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Documentation from the Agda user manual, for what isn't a name in a
-- library: keywords, symbols, pragmas, options, built-ins and primitives.
--
-- @scripts/gen-manual.py@ generates it from the manual's sources at build
-- time (as @data/manual.json@), and it is compiled into the server. Built
-- without it, there is none.
module Agdalicious.Manual (Entry (..), term, libraryLink) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.FileEmbed (embedFileIfExists)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data Entry = Entry
  { title, url :: Text
  , summary :: Text
  -- ^ its first paragraph
  , example :: Text
  -- ^ its first piece of Agda code
  , text :: Text
  -- ^ all of it (up to a length), as markdown
  }
  deriving (Generic, FromJSON)

data Manual = Manual
  { entries :: [Entry]
  , terms :: Map Text Int
  , libraries :: Map Text Text
  }
  deriving (Generic, FromJSON)

manual :: Manual
manual =
  maybe (Manual [] mempty mempty) id $
    Aeson.decodeStrict =<< $(embedFileIfExists "data/manual.json")

-- | What the manual says about a keyword, symbol, pragma (@OPTIONS@), option
-- (@--safe@), built-in (@BUILTIN NATURAL@) or primitive (@primNatToChar@).
term :: Text -> Maybe Entry
term t = (entries manual !!) <$> Map.lookup t (terms manual)

-- | The link to a definition in a library's HTML documentation, if it has any.
libraryLink :: Text -> Text -> Text -> Int -> Maybe Text
libraryLink name version modul offset = fill <$> Map.lookup name (libraries manual)
  where
    fill =
      T.replace "{version}" version
        . T.replace "{module}" modul
        . T.replace "{offset}" (T.pack (show offset))
