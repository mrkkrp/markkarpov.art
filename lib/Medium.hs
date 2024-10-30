{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Medium (Medium (..)) where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T

-- | Art mediums.
data Medium
  = OilOnCanvas
  | OilOnPaper
  | Watercolor
  | WatercolorAndInk
  | MixedMedia
  | CPencilsConteOnPaper
  | Photograph
  deriving (Eq, Show)

instance FromJSON Medium where
  parseJSON = withText "medium" $ \txt ->
    case txt of
      "oil_on_canvas" -> return OilOnCanvas
      "oil_on_paper" -> return OilOnPaper
      "watercolor" -> return Watercolor
      "watercolor_and_ink" -> return WatercolorAndInk
      "mixed_media" -> return MixedMedia
      "cpencils_conte_on_paper" -> return CPencilsConteOnPaper
      "photograph" -> return Photograph
      _ -> fail ("unknown medium: " ++ T.unpack txt)

instance ToJSON Medium where
  toJSON = toJSON . mediumName

mediumName :: Medium -> Text
mediumName = \case
  OilOnCanvas -> "oil on canvas"
  OilOnPaper -> "oil on paper"
  Watercolor -> "watercolor"
  WatercolorAndInk -> "watercolor and ink"
  MixedMedia -> "mixed media"
  CPencilsConteOnPaper -> "colored pencils and conté crayon on paper"
  Photograph -> "Photograph"
