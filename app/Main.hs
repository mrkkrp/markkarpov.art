{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Development.Shake hiding (Verbosity (..))
import Development.Shake.FilePath

----------------------------------------------------------------------------
-- Settings

-- | Top-level output directory of the site.
outdir :: FilePath
outdir = "_build"

----------------------------------------------------------------------------
-- Routing

-- | A route equipped with 'FilePattern' for input source files and a
-- function how to get output file name from input file name. The function
-- should not mess with 'outdir', that will be done for users automatically.
data Route
  = -- | Produced from inputs
    Ins FilePattern (FilePath -> FilePath)
  | -- | Generated, fixed file
    Gen FilePath
  | -- | Generated, pattern
    GenPat FilePath

-- | A helper for defining rules.
buildRoute ::
  -- | 'Route' we want to build
  Route ->
  -- | Input file, output file
  (FilePath -> FilePath -> Action ()) ->
  Rules ()
buildRoute (Ins pat mapOut') f = do
  let mapOut x = outdir </> mapOut' x
  action $
    getMatchingFiles pat >>= need . fmap mapOut
  inputMap <- fmap ($ ()) . newCache $ \() -> do
    ifiles <- getMatchingFiles pat
    return $ Map.fromList (zip (mapOut <$> ifiles) ifiles)
  mapOut pat %> \output -> do
    input <- (Map.! output) <$> inputMap
    f input output
buildRoute (Gen outFile') f = do
  let outFile = outdir </> outFile'
  want [outFile]
  outFile %> \output ->
    f output output
buildRoute (GenPat outFile') f = do
  let outFile = outdir </> outFile'
  outFile %> \output ->
    f output output

----------------------------------------------------------------------------
-- Routes

cssR,
  jsR,
  imgR,
  notFoundR,
  exhibitionsR,
  exhibitionR,
  artR,
  essaysR,
  essayR,
  contactR ::
    Route
cssR = Ins "static/css/*.css" id
jsR = Ins "static/js/*.js" id
imgR = Ins "static/img/**/*" id
notFoundR = Gen "404.html"
exhibitionsR = Gen "exhibitions.html"
exhibitionR = GenPat "exhibition/*.html"
artR = Gen "art.html"
essaysR = Gen "essays.html"
essayR = Ins "essay/*.md" (-<.> "html")
contactR = Ins "contact.md" (-<.> "html")

----------------------------------------------------------------------------
-- Types (site content)

-- | Information about an exhibition.
data Exhibition = Exhibition
  { exhibitionTitle :: !Text,
    exhibitionLocation :: !Text,
    exhibitionDescription :: !Text,
    exhibitionStart :: !Day,
    exhibitionEnd :: !Day,
    exhibitionArtworks :: Set ArtworkId
  }
  deriving (Eq, Show)

instance FromJSON Exhibition where
  parseJSON = withObject "exhibition" $ \o -> do
    exhibitionTitle <- o .: "title"
    exhibitionLocation <- o .: "location"
    exhibitionDescription <- o .: "description"
    exhibitionStart <- (o .: "start") >>= parseDay
    exhibitionEnd <- (o .: "end") >>= parseDay
    exhibitionArtworks <- Set.fromList <$> (o .: "artworks")
    return Exhibition {..}

instance ToJSON Exhibition where
  toJSON Exhibition {..} =
    object
      [ "title" .= exhibitionTitle,
        "location" .= exhibitionLocation,
        "description" .= exhibitionDescription,
        "start" .= exhibitionStart,
        "end" .= exhibitionEnd,
        "artworks" .= toJSON exhibitionArtworks
      ]

-- | Artwork id.
newtype ArtworkId = ArtworkId Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Information about an artwork.
data Artwork = Artwork
  { artworkId :: ArtworkId,
    artworkTitle :: !Text,
    artworkDescription :: !(Maybe Text),
    artworkMedium :: !Medium,
    artworkHeight :: !Int,
    artworkWidth :: !Int,
    artworkDate :: !Day
  }
  deriving (Eq, Show)

instance FromJSON Artwork where
  parseJSON = withObject "artwork" $ \o -> do
    artworkId <- o .: "id"
    artworkTitle <- o .: "title"
    artworkDescription <- o .:? "description"
    artworkMedium <- o .: "medium"
    artworkHeight <- o .: "height"
    artworkWidth <- o .: "width"
    artworkDate <- (o .: "date") >>= parseDay
    return Artwork {..}

instance ToJSON Artwork where
  toJSON Artwork {..} =
    object
      [ "id" .= artworkId,
        "title" .= artworkTitle,
        "description" .= artworkDescription,
        "medium" .= artworkMedium,
        "height" .= artworkHeight,
        "width" .= artworkWidth,
        "date" .= artworkDate
      ]

-- | Art mediums.
data Medium
  = OilOnCanvas
  | OilOnPaper
  deriving (Eq, Show)

instance FromJSON Medium where
  parseJSON = withText "medium" $ \txt ->
    case txt of
      "oil_on_canvas" -> return OilOnCanvas
      "oil_on_paper" -> return OilOnPaper
      _ -> fail ("unknown medium: " ++ T.unpack txt)

instance ToJSON Medium where
  toJSON = toJSON . mediumName

-- | Information about an essay.
data Essay = Essay
  { essayTitle :: !Text,
    essayPublished :: !Day,
    essayUpdated :: !(Maybe Day),
    essayFile :: !FilePath
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
    let essayFile = ""
    return Essay {..}

instance ToJSON Essay where
  toJSON Essay {..} =
    object
      [ "title" .= essayTitle,
        "published" .= renderDay essayPublished,
        "updated" .= fmap renderDay essayUpdated,
        "file" .= ("/" ++ essayFile)
      ]

----------------------------------------------------------------------------
-- Menu items

-- | Menu items.
data MenuItem
  = Exhibitions
  | Art
  | Essays
  | Contact
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Get human-readable title of 'MenuItem'.
menuItemTitle :: MenuItem -> Text
menuItemTitle = \case
  Exhibitions -> "Exhibitions"
  Art -> "Art"
  Essays -> "Essays"
  Contact -> "Contact"

----------------------------------------------------------------------------
-- Build system

main :: IO ()
main = return ()

----------------------------------------------------------------------------
-- Helpers

getMatchingFiles :: FilePattern -> Action [FilePath]
getMatchingFiles = getDirectoryFiles "" . pure

-- | Human-readable medium name.
mediumName :: Medium -> Text
mediumName = \case
  OilOnCanvas -> "oil on canvas"
  OilOnPaper -> "oil on paper"

parseDay :: (MonadFail m) => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%B %e, %Y" . T.unpack

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%B %e, %Y"
