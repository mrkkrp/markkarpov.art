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
import Utils (renderDay)

-- | Information about an artwork.
data Artwork = Artwork
  { artworkId :: ArtworkId,
    artworkTitle :: !(Maybe Text),
    artworkDescription :: !(Maybe Text),
    artworkMedium :: !Medium,
    artworkDimensions :: !(Maybe (Int, Int))
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
        "date" .= renderDay (Artwork.Id.date artworkId)
      ]

-- | Project artwork's creation date.
date :: Artwork -> Day
date = Artwork.Id.date . artworkId
