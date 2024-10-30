{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Artwork
  ( Artwork (..),
  )
where

import Artwork.Id (ArtworkId (..))
import Data.Aeson
import Data.Text (Text)
import Data.Time
import Medium (Medium (..))
import Utils (parseDay, renderDay)

-- | Information about an artwork.
data Artwork = Artwork
  { artworkId :: ArtworkId,
    artworkTitle :: !(Maybe Text),
    artworkDescription :: !(Maybe Text),
    artworkMedium :: !Medium,
    artworkDimensions :: !(Maybe (Int, Int)),
    artworkDate :: !Day
  }
  deriving (Eq, Show)

instance FromJSON Artwork where
  parseJSON = withObject "artwork" $ \o -> do
    artworkId <- o .: "id"
    artworkTitle <- o .:? "title"
    artworkDescription <- o .:? "description"
    artworkMedium <- o .: "medium"
    artworkDimensions <- case artworkMedium of
      Photograph -> return Nothing
      _ -> do
        artworkHeight <- o .: "height"
        artworkWidth <- o .: "width"
        return $ Just (artworkHeight, artworkWidth)
    artworkDate <- (o .: "date") >>= parseDay
    return Artwork {..}

instance ToJSON Artwork where
  toJSON Artwork {..} =
    object
      [ "id" .= artworkId,
        "title" .= artworkTitle,
        "description" .= artworkDescription,
        "medium" .= artworkMedium,
        "height" .= (fst <$> artworkDimensions),
        "width" .= (snd <$> artworkDimensions),
        "date" .= renderDay artworkDate
      ]
