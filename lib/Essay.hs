{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Essay (Essay (..)) where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Utils (parseDay, renderDay)

-- | Information about an essay.
data Essay = Essay
  { essayTitle :: !Text,
    essayPublished :: !Day,
    essayUpdated :: !(Maybe Day)
  }
  deriving (Eq, Show)

instance FromJSON Essay where
  parseJSON = withObject "essay" $ \o -> do
    essayTitle <- o .: "title"
    essayPublished <- (o .: "date") >>= (.: "published") >>= parseDay
    essayUpdated <-
      (o .: "date")
        >>= (.:? "updated")
        >>= maybe (pure Nothing) (fmap Just . parseDay)
    return Essay {..}

instance ToJSON Essay where
  toJSON Essay {..} =
    object
      [ "title" .= essayTitle,
        "published" .= renderDay essayPublished,
        "updated" .= fmap renderDay essayUpdated
      ]
