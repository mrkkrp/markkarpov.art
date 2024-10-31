{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Artwork
  ( Artwork (..),
    date,
  )
where

import Artwork.Id (ArtworkId)
import Artwork.Id qualified
import Data.Aeson
import Data.Text (Text)
import Data.Time
import Medium (Medium (..))
import PhotoMetadata (PhotoMetadata)
import Utils (renderDay)

-- | Information about an artwork.
data Artwork = Artwork
  { artworkId :: ArtworkId,
    artworkTitle :: !(Maybe Text),
    artworkMedium :: !Medium,
    artworkDimensions :: !(Maybe (Int, Int)),
    artworkPhotoMetadata :: !(Maybe PhotoMetadata)
  }

instance FromJSON Artwork where
  parseJSON = withObject "artwork" $ \o -> do
    artworkId <- o .: "id"
    artworkTitle <- o .:? "title"
    artworkMedium <- o .: "medium"
    case artworkMedium of
      Photograph -> do
        let artworkDimensions = Nothing
        artworkPhotoMetadata <- parseJSON (Object o)
        return Artwork {..}
      _ -> do
        artworkHeight <- o .: "height"
        artworkWidth <- o .: "width"
        let artworkDimensions = Just (artworkHeight, artworkWidth)
            artworkPhotoMetadata = Nothing
        return Artwork {..}

instance ToJSON Artwork where
  toJSON Artwork {..} =
    object
      [ "id" .= artworkId,
        "title" .= artworkTitle,
        "medium" .= artworkMedium,
        "height" .= (fst <$> artworkDimensions),
        "width" .= (snd <$> artworkDimensions),
        "photo_metadata" .= toJSON artworkPhotoMetadata,
        "date" .= renderDay (Artwork.Id.date artworkId)
      ]

-- | Project artwork's creation date.
date :: Artwork -> Day
date = Artwork.Id.date . artworkId
