-- | Go to definition, from the @definitionSite@s in Agda's highlighting.
--
-- Agda counts in characters from 1 (@offset@); LSP counts lines and UTF-16
-- code units from 0 ('Position'). Texts are the 'Rope's the VFS keeps.
module Agdalicious.Definition (Name (..), names, nameAt, offsetOf, positionOf) where

import Control.Lens ((^..), (^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _Integral, _String)
import Data.List (find)
import Data.Text qualified as T
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Data.Text.Utf16.Lines qualified as Lines
import Language.LSP.Protocol.Types (Position (..))

-- | A name in a document and where it is defined.
data Name = Name
  { from, to :: Int
  -- ^ its offsets (end exclusive)
  , site :: FilePath
  , siteOffset :: Int
  }

-- | The names in the highlighting of a load's responses.
names :: [Aeson.Value] -> [Name]
names responses =
  [ Name f t (T.unpack p) o
  | r <- responses
  , r ^? key "kind" . _String == Just "HighlightingInfo"
  , n <- r ^.. key "info" . key "payload" . values
  , [f, t] <- [n ^.. key "range" . values . _Integral]
  , Just p <- [n ^? key "definitionSite" . key "filepath" . _String]
  , Just o <- [n ^? key "definitionSite" . key "position" . _Integral]
  ]

nameAt :: Int -> [Name] -> Maybe Name
nameAt off = find (\n -> from n <= off && off < to n)

offsetOf :: Position -> Rope -> Maybe Int
offsetOf (Position l c) t =
  (+ 1) . fromIntegral . Rope.charLength . fst <$> Rope.utf16SplitAtPosition (Lines.Position (fromIntegral l) (fromIntegral c)) t

positionOf :: Int -> Rope -> Position
positionOf off t = Position (fromIntegral l) (fromIntegral c)
  where
    Lines.Position l c = Rope.utf16LengthAsPosition (fst (Rope.charSplitAt (fromIntegral (off - 1)) t))
