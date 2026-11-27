-- | What a load tells about a document: its tokens, from Agda's highlighting,
-- and its holes.
--
-- Agda counts in characters from 1 (an offset); LSP counts lines and UTF-16
-- code units from 0 ('Position'). Texts are the 'Rope's the VFS keeps.
module Agdalicious.Document
  ( Document (..)
  , Token (..)
  , Goal (..)
  , fromResponses
  , tokenAt
  , goalAt
  , offsetOf
  , positionOf
  , rangeOf
  , slice
  ) where

import Control.Lens ((^..), (^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _Integral, _String)
import Data.List (find, minimumBy)
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Data.Text.Utf16.Lines qualified as Lines
import Language.LSP.Protocol.Types (Position (..), Range (..))

data Document = Document
  { tokens :: [Token]
  , goals :: [Goal]
  }

-- | A highlighted piece of a document.
data Token = Token
  { from, to :: Int
  -- ^ its offsets (end exclusive)
  , atoms :: [Text]
  -- ^ what it is: @keyword@, @function@, @bound@, @module@, ...
  , site :: Maybe (FilePath, Int)
  -- ^ where it is defined, if it is a name
  }

-- | A hole, by its interaction point number.
data Goal = Goal {goal :: Int, goalFrom, goalTo :: Int}

fromResponses :: [Aeson.Value] -> Document
fromResponses responses =
  Document
    { tokens =
        [ Token f t (n ^.. key "atoms" . values . _String) site
        | r <- responses
        , r ^? key "kind" . _String == Just "HighlightingInfo"
        , n <- r ^.. key "info" . key "payload" . values
        , [f, t] <- [n ^.. key "range" . values . _Integral]
        , let site = (,) . T.unpack <$> n ^? key "definitionSite" . key "filepath" . _String
                <*> n ^? key "definitionSite" . key "position" . _Integral
        ]
    , goals =
        [ Goal i f t
        | r <- responses
        , r ^? key "kind" . _String == Just "InteractionPoints"
        , p <- r ^.. key "interactionPoints" . values
        , Just i <- [p ^? key "id" . _Integral]
        , Just f <- [p ^? key "range" . values . key "start" . key "pos" . _Integral]
        , Just t <- [p ^? key "range" . values . key "end" . key "pos" . _Integral]
        ]
    }

-- | The innermost token at an offset (a name, if there are several).
tokenAt :: Int -> [Token] -> Maybe Token
tokenAt off ts = case [t | t <- ts, from t <= off, off < to t] of
  [] -> Nothing
  hits -> Just (minimumBy (comparing (\t -> (to t - from t, isNothing (site t)))) hits)

goalAt :: Int -> [Goal] -> Maybe Goal
goalAt off = find (\g -> goalFrom g <= off && off < goalTo g)

offsetOf :: Position -> Rope -> Maybe Int
offsetOf (Position l c) t =
  (+ 1) . fromIntegral . Rope.charLength . fst <$> Rope.utf16SplitAtPosition (Lines.Position (fromIntegral l) (fromIntegral c)) t

positionOf :: Int -> Rope -> Position
positionOf off t = Position (fromIntegral l) (fromIntegral c)
  where
    Lines.Position l c = Rope.utf16LengthAsPosition (fst (Rope.charSplitAt (fromIntegral (off - 1)) t))

rangeOf :: Int -> Int -> Rope -> Range
rangeOf f t r = Range (positionOf f r) (positionOf t r)

-- | The text between two offsets.
slice :: Int -> Int -> Rope -> Text
slice f t = Rope.toText . fst . Rope.charSplitAt (fromIntegral (t - f)) . snd . Rope.charSplitAt (fromIntegral (f - 1))
