{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash qualified as Hash
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.List (find, foldl1', sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Time
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import Development.Shake hiding (Verbosity (..))
import Development.Shake.FilePath
import GHC.Generics (Generic)
import Lucid qualified as L
import Text.MMark qualified as MMark
import Text.MMark.Extension qualified as Ext
import Text.MMark.Extension.Common qualified as Ext
import Text.Megaparsec qualified as M
import Text.Mustache
import Text.URI (URI)
import Text.URI qualified as URI
import Text.URI.Lens (uriPath, uriScheme)
import Text.URI.QQ (scheme)

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

essayPattern :: FilePath
essayPattern = "essay/*.md"

essayMapOut :: FilePath -> FilePath
essayMapOut = (-<.> "html")

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
essayR = Ins essayPattern essayMapOut
contactR = Ins "contact.md" (-<.> "html")

----------------------------------------------------------------------------
-- Types (site content)

-- | Information about an exhibition.
data Exhibition = Exhibition
  { exhibitionTitle :: !Text,
    exhibitionLocation :: !Text,
    exhibitionDescription :: !Text,
    exhibitionLink :: !Text,
    exhibitionStart :: !Day,
    exhibitionEnd :: !Day,
    exhibitionArtworks :: !(Set ArtworkId),
    exhibitionFile :: !FilePath
  }
  deriving (Eq, Show)

deriving stock instance Generic Day

deriving anyclass instance Binary Day

instance FromJSON Exhibition where
  parseJSON = withObject "exhibition" $ \o -> do
    exhibitionTitle <- o .: "title"
    exhibitionLocation <- o .: "location"
    exhibitionDescription <- o .: "description"
    exhibitionLink <- o .: "link"
    exhibitionStart <- (o .: "start") >>= parseDay
    exhibitionEnd <- (o .: "end") >>= parseDay
    exhibitionArtworks <- Set.fromList <$> (o .: "artworks")
    let exhibitionDigest :: Hash.Digest SHA256
        exhibitionDigest =
          (Hash.hash . BS.toStrict . Binary.encode)
            (exhibitionLink, exhibitionStart, exhibitionEnd)
        exhibitionFile =
          outdir </> "exhibition" </> show exhibitionDigest <.> "html"
    return Exhibition {..}

instance ToJSON Exhibition where
  toJSON Exhibition {..} =
    object
      [ "title" .= exhibitionTitle,
        "location" .= exhibitionLocation,
        "description" .= exhibitionDescription,
        "link" .= exhibitionLink,
        "start" .= exhibitionStart,
        "end" .= exhibitionEnd,
        "artworks" .= toJSON exhibitionArtworks,
        "file" .= exhibitionFile
      ]

-- | Artwork id.
newtype ArtworkId = ArtworkId Text
  deriving (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | Information about an artwork.
data Artwork = Artwork
  { artworkId :: ArtworkId,
    artworkTitle :: !(Maybe Text),
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
    artworkTitle <- o .:? "title"
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
        "date" .= renderDay artworkDate
      ]

-- | Art mediums.
data Medium
  = OilOnCanvas
  | OilOnPaper
  | Watercolor
  | Photo
  | MixedMedia
  deriving (Eq, Show)

instance FromJSON Medium where
  parseJSON = withText "medium" $ \txt ->
    case txt of
      "oil_on_canvas" -> return OilOnCanvas
      "oil_on_paper" -> return OilOnPaper
      "watercolor" -> return Watercolor
      "photo" -> return Photo
      "mixed_media" -> return MixedMedia
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
  envCache <- cacheYamlFile "env.yaml"
  exhibitionCache :: Action [Exhibition] <-
    fmap (sortOn (Down . exhibitionStart)) <$> cacheYamlFile "exhibitions.yaml"
  artworkCache :: Action [Artwork] <-
    fmap (sortOn (Down . artworkId)) <$> cacheYamlFile "artworks.yaml"
  templateCache <- newCache' $ \() -> do
    let templateP = "templates/*.mustache"
    getMatchingFiles templateP >>= need
    liftIO (compileMustacheDir "default" (takeDirectory templateP))
  getMd <- newCache $ \path -> do
    env <- envCache
    getMdHelper env path
  essayCache <- newCache' $ \() -> do
    srcs <- getMatchingFiles essayPattern
    fmap (sortOn essayPublished) . forM srcs $ \src -> do
      need [src]
      v <- getMd src >>= interpretValue . fst
      return v {essayFile = essayMapOut src}
  let justFromTemplate ::
        Either Text MenuItem ->
        PName ->
        FilePath ->
        Action ()
      justFromTemplate etitle template output = do
        env <- envCache
        ts <- templateCache
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
  buildRoute exhibitionsR $ \_ output -> do
    env <- envCache
    exhibitions <- exhibitionCache
    templates <- templateCache
    need (exhibitionFile <$> exhibitions)
    -- TODO display in upcoming, current, and past sections
    -- TODO group by years
    renderAndWrite
      templates
      ["exhibitions", "default"]
      Nothing
      [ menuItem Exhibitions env,
        provideAs "exhibition" exhibitions,
        mkTitle Exhibitions
      ]
      output
  buildRoute exhibitionR $ \_ output -> do
    env <- envCache
    exhibitions <- exhibitionCache
    artworks <- artworkCache
    templates <- templateCache
    thisExhibition <- case find ((== output) . exhibitionFile) exhibitions of
      Nothing ->
        fail $
          "Trying to build " ++ output ++ " but no matching exhibitions found"
      Just x -> return x
    let relevantArtworks =
          filter
            ((`Set.member` exhibitionArtworks thisExhibition) . artworkId)
            artworks
    renderAndWrite
      templates
      ["exhibition", "default"]
      Nothing
      [ menuItem Exhibitions env,
        toJSON thisExhibition,
        provideAs "artwork" relevantArtworks
      ]
      output
  buildRoute artR $ \_ output -> do
    env <- envCache
    artworks <- artworkCache
    templates <- templateCache
    renderAndWrite
      templates
      ["artworks", "default"]
      Nothing
      [ menuItem Art env,
        provideAs "artwork" artworks,
        mkTitle Art
      ]
      output
  buildRoute essaysR $ \_ output -> do
    env <- envCache
    essays <- essayCache
    templates <- templateCache
    renderAndWrite
      templates
      ["essays", "default"]
      Nothing
      [ menuItem Essays env,
        provideAs "essay" essays,
        mkTitle Essays
      ]
      output
  buildRoute essayR $ \input output -> do
    env <- envCache
    templates <- templateCache
    need [input]
    (v, content) <- getMd input
    renderAndWrite
      templates
      ["essay", "default"]
      (Just content)
      [ menuItem Essays env,
        v
      ]
      output
  buildRoute contactR $ \input output -> do
    env <- envCache
    templates <- templateCache
    need [input]
    (v, content) <- getMd input
    renderAndWrite
      templates
      ["contact", "default"]
      (Just content)
      [ menuItem Contact env,
        mkTitle Contact,
        v
      ]
      output

----------------------------------------------------------------------------
-- Custom MMark extensions

addTableClasses :: MMark.Extension
addTableClasses = Ext.blockRender $ \old block ->
  case block of
    t@(Ext.Table _ _) -> L.with (old t) [L.class_ "table table-striped"]
    other -> old other

addImageClasses :: MMark.Extension
addImageClasses = Ext.inlineRender $ \old inline ->
  case inline of
    i@Ext.Image {} -> L.with (old i) [L.class_ "img-fluid"]
    other -> old other

provideSocialUrls :: Value -> MMark.Extension
provideSocialUrls v = Ext.inlineTrans $ \case
  l@(Ext.Link inner uri mtitle) ->
    if URI.uriScheme uri == Just [scheme|social|]
      then case uri ^. uriPath of
        [x] ->
          case v
            ^? key "social"
              . key (Key.fromText (URI.unRText x))
              . _String
              . getURI of
            Nothing -> Ext.Plain "!lookup failed!"
            Just t ->
              if Ext.asPlainText inner == "x"
                then
                  Ext.Link
                    (Ext.Plain (URI.render t) :| [])
                    ((uriScheme ?~ [scheme|mailto|]) t)
                    mtitle
                else Ext.Link inner t mtitle
        _ -> l
      else l
  other -> other

getURI :: Traversal' Text URI
getURI f txt = maybe txt URI.render <$> traverse f (URI.mkURI txt :: Maybe URI)

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
  Watercolor -> "watercolor"
  Photo -> "photo"
  MixedMedia -> "mixed media"

parseDay :: (MonadFail m) => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%F" . T.unpack

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%d.%m.%Y"

mkContext :: [Value] -> Value
mkContext = foldl1' f
  where
    f (Object m0) (Object m1) = Object (KeyMap.union m0 m1)
    f _ _ = error "context merge failed"

mkTitle :: MenuItem -> Value
mkTitle = provideAs "title" . menuItemTitle

provideAs :: (ToJSON v) => Text -> v -> Value
provideAs k v = Object (KeyMap.singleton (Key.fromText k) (toJSON v))

newCache' :: (() -> Action v) -> Rules (Action v)
newCache' = fmap ($ ()) . newCache

cacheYamlFile :: (FromJSON v) => FilePath -> Rules (Action v)
cacheYamlFile yamlFile = newCache' $ \() -> do
  need [yamlFile]
  r <- liftIO (Y.decodeFileEither yamlFile)
  case r of
    Left err -> fail (Y.prettyPrintParseException err)
    Right value -> return value

getMdHelper :: Value -> FilePath -> Action (Value, TL.Text)
getMdHelper env path = do
  txt <- liftIO (T.readFile path)
  case MMark.parse path txt of
    Left bundle -> fail (M.errorBundlePretty bundle)
    Right doc -> do
      let toc = MMark.runScanner doc (Ext.tocScanner (\x -> x > 1 && x < 5))
          r =
            MMark.useExtensions
              [ Ext.fontAwesome,
                Ext.footnotes,
                Ext.kbd,
                Ext.linkTarget,
                Ext.mathJax (Just '$'),
                Ext.obfuscateEmail "protected-email",
                Ext.punctuationPrettifier,
                Ext.ghcSyntaxHighlighter,
                Ext.skylighting,
                Ext.toc "toc" toc,
                addTableClasses,
                addImageClasses,
                provideSocialUrls env
              ]
              doc
          v = fromMaybe (object []) (MMark.projectYaml doc)
      return (v, L.renderText (MMark.render r))

interpretValue :: (FromJSON v) => Value -> Action v
interpretValue v =
  case fromJSON v of
    Error str -> fail str
    Success a -> return a
