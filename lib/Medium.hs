{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Medium
  ( Medium (..),
    summary,
  )
where

import Data.Aeson
import Data.List qualified
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | Art mediums.
data Medium
  = OilOnCanvas
  | Watercolor
  | WatercolorAndInk
  | MixedMedia
  | CPencilsConteOnPaper
  | Photograph
  deriving (Eq, Ord, Show)

instance FromJSON Medium where
  parseJSON = withText "medium" $ \txt ->
    case txt of
      "oil_on_canvas" -> return OilOnCanvas
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
  Watercolor -> "watercolor"
  WatercolorAndInk -> "watercolor and ink"
  MixedMedia -> "mixed media"
  CPencilsConteOnPaper -> "colored pencils and conté crayon on paper"
  Photograph -> "photograph"

mediumNamePlural :: Medium -> Text
mediumNamePlural = \case
  OilOnCanvas -> "oils"
  Watercolor -> "watercolors"
  WatercolorAndInk -> "watercolors with ink"
  MixedMedia -> "mixed media"
  CPencilsConteOnPaper -> "colored pencils and conté crayon on paper"
  Photograph -> "photographs"

summary :: [a] -> (a -> Medium) -> Text
summary xs getMedium =
  if n == 0
    then "No artworks"
    else case mediumsList of
      [_] -> T.pack (show n) <> " " <> artworks
      _ -> T.pack (show n) <> " artworks: " <> mediumsText
  where
    n = length xs
    artworks = if n == 1 then "artwork" else "artworks"
    mediumsText =
      case mediumsList of
        [] -> error "should not happen"
        [(x, nx)] ->
          renderMedium x nx
        ((x, nx) : (y, ny) : _) ->
          joinWithCommas
            ( renderMedium x nx
                :| ([renderMedium y ny] ++ renderOthers (n - nx - ny))
            )
    mediumsList =
      Data.List.sortBy
        cmp'
        (equipWithCounts <$> NonEmpty.groupAllWith id (getMedium <$> xs))
    equipWithCounts ys@(y :| _) = (y, length ys)
    cmp' (x, nx) (y, ny) =
      case Down nx `compare` Down ny of
        EQ -> Down x `compare` Down y
        r -> r
    renderMedium m nm =
      T.pack (show nm)
        <> " "
        <> if nm == 1 then mediumName m else mediumNamePlural m
    renderOthers n' =
      if n' == 0
        then []
        else
          if n' == 1
            then ["1 other"]
            else [T.pack (show n') <> " others"]
    joinWithCommas (x :| []) = x
    joinWithCommas (x0 :| [x1]) = x0 <> " and " <> x1
    joinWithCommas xs0 =
      case NonEmpty.reverse xs0 of
        (x :| xs1) -> T.intercalate ", " (reverse xs1) <> ", and " <> x
