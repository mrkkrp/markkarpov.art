{-# LANGUAGE DerivingStrategies #-}

module Artwork.Id (ArtworkId (..)) where

import Data.Aeson
import Data.Text (Text)

-- | Artwork id.
newtype ArtworkId = ArtworkId Text
  deriving (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)
