{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Exhibition
  ( Exhibition (..),
  )
where

import Artwork.Id (ArtworkId)
import Crypto.Hash qualified as Hash
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Time
import GHC.Generics (Generic)
import Utils (parseDay, renderDay)

-- | Information about an exhibition.
data Exhibition = Exhibition
  { exhibitionTitle :: !Text,
    exhibitionLocation :: !Text,
    exhibitionDescriptionRaw :: !Text,
    exhibitionDescriptionRendered :: !TL.Text,
    exhibitionLink :: !Text,
    exhibitionStart :: !Day,
    exhibitionEnd :: !Day,
    exhibitionArtworks :: !(Set ArtworkId),
    exhibitionDigest :: !(Hash.Digest SHA256)
  }
  deriving (Eq, Show)

deriving stock instance Generic Day

deriving anyclass instance Binary Day

instance FromJSON Exhibition where
  parseJSON = withObject "exhibition" $ \o -> do
    exhibitionTitle <- o .: "title"
    exhibitionLocation <- o .: "location"
    exhibitionDescriptionRaw <- o .: "description"
    let exhibitionDescriptionRendered = TL.empty
    exhibitionLink <- o .: "link"
    exhibitionStart <- (o .: "start") >>= parseDay
    exhibitionEnd <- (o .: "end") >>= parseDay
    exhibitionArtworks <- Set.fromList <$> (o .: "artworks")
    let exhibitionDigest =
          (Hash.hash . BS.toStrict . Binary.encode)
            (exhibitionLink, exhibitionStart, exhibitionEnd)
    return Exhibition {..}

instance ToJSON Exhibition where
  toJSON Exhibition {..} =
    object
      [ "title" .= exhibitionTitle,
        "location" .= exhibitionLocation,
        "description_raw" .= exhibitionDescriptionRaw,
        "description_rendered" .= exhibitionDescriptionRendered,
        "link" .= exhibitionLink,
        "start" .= renderDay exhibitionStart,
        "end" .= renderDay exhibitionEnd,
        "artworks" .= toJSON exhibitionArtworks,
        "digest" .= show exhibitionDigest
      ]
