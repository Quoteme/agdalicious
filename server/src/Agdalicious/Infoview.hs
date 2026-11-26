{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Wire format of the custom @agda/infoview@ request.
--
-- The client sends the document and cursor position; the server answers with
-- whatever the infoview should show there (goals, context, messages).
module Agdalicious.Infoview
  ( infoviewMethod
  , InfoviewParams (..)
  , InfoviewResult (..)
  , Goal (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.LSP.Protocol.Types (Position, TextDocumentIdentifier)

-- | Name of the custom LSP method. Keep in sync with @nvim/lua/agdalicious/infoview.lua@.
infoviewMethod :: Text
infoviewMethod = "agda/infoview"

data InfoviewParams = InfoviewParams
  { textDocument :: TextDocumentIdentifier
  , position :: Position
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Goal = Goal
  { name :: Text
  -- ^ e.g. @?0@
  , goalType :: Text
  , context :: [Text]
  -- ^ hypotheses, already pretty-printed (@x : ℕ@)
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data InfoviewResult = InfoviewResult
  { goals :: [Goal]
  , messages :: [Text]
  }
  deriving (Show, Generic, FromJSON, ToJSON)
