{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens hiding ((.=), (<.>))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens
import Data.List (foldl', foldl1', sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Time
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import Development.Shake hiding (Verbosity (..))
import Development.Shake.FilePath
import Text.Mustache

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
main = shakeArgs shakeOptions $ do
  -- Helpers
  phony "clean" $ do
    putNormal ("Cleaning files in " ++ outdir)
    removeFilesAfter outdir ["//*"]
  commonEnv <- fmap ($ ()) . newCache $ \() -> do
    let commonEnvFile = "env.yaml"
    need [commonEnvFile]
    r <- liftIO (Y.decodeFileEither commonEnvFile)
    case r of
      Left err -> fail (Y.prettyPrintParseException err)
      Right value -> return value
  templates <- fmap ($ ()) . newCache $ \() -> do
    let templateP = "templates/*.mustache"
    getMatchingFiles templateP >>= need
    liftIO (compileMustacheDir "default" (takeDirectory templateP))
  let justFromTemplate ::
        Either Text MenuItem ->
        PName ->
        FilePath ->
        Action ()
      justFromTemplate etitle template output = do
        env <- commonEnv
        ts <- templates
        renderAndWrite
          ts
          [template, "default"]
          Nothing
          [ either (const env) (`menuItem` env) etitle,
            provideAs "title" (either id menuItemTitle etitle)
          ]
          output

  -- Page implementations
  buildRoute cssR copyFile'
  buildRoute jsR copyFile'
  buildRoute imgR copyFile'
  buildRoute notFoundR $ \_ output ->
    justFromTemplate (Left "404 Not Found") "404" output

----------------------------------------------------------------------------
-- Helpers

getMatchingFiles :: FilePattern -> Action [FilePath]
getMatchingFiles = getDirectoryFiles "" . pure

selectTemplate :: PName -> Template -> Template
selectTemplate name t = t {templateActual = name}

renderAndWrite ::
  (MonadIO m) =>
  -- | Templates to use
  Template ->
  -- | Names of templates, in order
  [PName] ->
  -- | First inner value to interpolate
  Maybe TL.Text ->
  -- | Rendering context
  [Value] ->
  -- | File path where to write rendered file
  FilePath ->
  m ()
renderAndWrite ts pnames minner context out =
  liftIO . TL.writeFile out $
    foldl f (fromMaybe TL.empty minner) pnames
  where
    f inner pname =
      renderMustache
        (selectTemplate pname ts)
        (mkContext (provideAs "inner" inner : context))

menuItem :: MenuItem -> Value -> Value
menuItem item = over (key "main_menu" . _Array) . V.map $ \case
  Object m ->
    Object $
      if KeyMap.lookup "title" m == (Just . String . menuItemTitle) item
        then KeyMap.insert "active" (Bool True) m
        else m
  v -> v

mediumName :: Medium -> Text
mediumName = \case
  OilOnCanvas -> "oil on canvas"
  OilOnPaper -> "oil on paper"

parseDay :: (MonadFail m) => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%B %e, %Y" . T.unpack

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%B %e, %Y"

mkContext :: [Value] -> Value
mkContext = foldl1' f
  where
    f (Object m0) (Object m1) = Object (KeyMap.union m0 m1)
    f _ _ = error "context merge failed"

provideAs :: (ToJSON v) => Text -> v -> Value
provideAs k v = Object (KeyMap.singleton (Key.fromText k) (toJSON v))
