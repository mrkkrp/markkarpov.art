module Artwork.Id
  ( ArtworkId,
    date,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time

-- | Artwork id.
data ArtworkId = ArtworkId Text Day
  deriving (Eq, Ord, Show)

instance FromJSON ArtworkId where
  parseJSON = withText "artwork id" $ \txt ->
    case parseTimeM False defaultTimeLocale "%0Y%m%d" (T.unpack (T.take 8 txt)) of
      Nothing -> fail $ T.unpack txt ++ " is not a valid artwork identifier (it must start with YYYYMMDD)"
      Just day -> pure (ArtworkId txt day)

instance ToJSON ArtworkId where
  toJSON (ArtworkId txt _) = toJSON txt

-- | Project artwork's creation date.
date :: ArtworkId -> Day
date (ArtworkId _ day) = day
