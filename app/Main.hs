{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Artwork (Artwork (..))
import Artwork qualified
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash qualified as Hash
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens
import Data.Function (on)
import Data.List (find, foldl1', groupBy, sortOn)
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
import Essay (Essay (..))
import Exhibition (Exhibition (..))
import Exhibition.PerYear (ExhibitionPerYear (..))
import Markdown qualified
import Route (Route)
import Route qualified
import Text.Mustache
import Utils (getMatchingFiles, newCache')

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
  envCache <- cacheYamlFile "env.yaml"
  exhibitionCache :: Action [Exhibition] <-
    fmap (sortOn (Down . exhibitionStart)) <$> cacheYamlFile "exhibitions.yaml"
  artworkCache :: Action [Artwork] <-
    fmap (sortOn artworkId) <$> cacheYamlFile "artworks.yaml"
  templateCache <- newCache' $ \() -> do
    let templatePattern = "templates/*.mustache"
    getMatchingFiles templatePattern >>= need
    liftIO (compileMustacheDir "default" (takeDirectory templatePattern))
  getMd <- newCache $ \path -> do
    env <- envCache
    getMdHelper env path
  essayCache <- newCache' $ \() -> do
    srcs <- getMatchingFiles Route.essayPattern
    fmap (sortOn (essayPublished . Route.unWithPage)) . forM srcs $ \src -> do
      need [src]
      v :: Essay <- getMd src >>= interpretValue . fst
      return (Route.withPage Route.essay src v)
  let routeArtYear :: Route Year
      routeArtYear =
        Route.artYear $
          getArtworkYears <$> artworkCache
      routeExhibition :: Route (Hash.Digest SHA256)
      routeExhibition =
        Route.exhibition $
          Set.fromList . fmap exhibitionDigest <$> exhibitionCache
      justFromTemplate ::
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
  Route.rule Route.notFound $ \() output ->
    justFromTemplate (Left "404 Not Found") "404" output
  Route.rule Route.exhibitions $ \() output -> do
    env <- envCache
    exhibitions <- exhibitionCache
    templates <- templateCache
    let exhibitionsByYear =
          fmap
            ( \xs ->
                ExhibitionPerYear
                  (exhibitionYear (head xs))
                  (wrapWithOutput <$> xs)
            )
            (groupBy ((==) `on` exhibitionYear) exhibitions)
        wrapWithOutput x = Route.withPage routeExhibition (exhibitionDigest x) x
    renderAndWrite
      templates
      ["exhibitions", "default"]
      Nothing
      [ menuItem Exhibitions env,
        provideAs "exhibitions_by_year" exhibitionsByYear,
        mkTitle Exhibitions
      ]
      output
  Route.rule routeExhibition $ \thisExhibitionDigest output -> do
    env <- envCache
    exhibitions <- exhibitionCache
    artworks <- artworkCache
    templates <- templateCache
    thisExhibition <- case find ((== thisExhibitionDigest) . exhibitionDigest) exhibitions of
      Nothing ->
        fail $
          "Trying to build " ++ output ++ " but no matching exhibitions found"
      Just x -> do
        descRendered <-
          snd
            <$> Markdown.render
              env
              (exhibitionDescriptionRaw x)
              ("Description of " ++ T.unpack (exhibitionTitle x))
        return x {exhibitionDescriptionRendered = descRendered}
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
  Route.rule Route.art $ \() output -> do
    env <- envCache
    templates <- templateCache
    years <- getArtworkYears <$> artworkCache
    let presentYear year = Route.withPage routeArtYear year year
    renderAndWrite
      templates
      ["art", "default"]
      Nothing
      [ menuItem Art env,
        provideAs "year" (presentYear <$> Set.toDescList years),
        mkTitle Art
      ]
      output
  Route.rule routeArtYear $ \thisYear output -> do
    env <- envCache
    artworks <- artworkCache
    templates <- templateCache
    let prefaceFile = "art-per-year" </> show thisYear <.> "md"
        thisYearArtworks = filterArtworksByYear thisYear artworks
    preface <- snd <$> getMd prefaceFile
    renderAndWrite
      templates
      ["artworks", "default"]
      (Just preface)
      [ menuItem Art env,
        provideAs "artwork" thisYearArtworks,
        provideAs "this_year" thisYear,
        provideAs "artworks_total" (length thisYearArtworks),
        provideAs "title" (menuItemTitle Art <> " " <> T.pack (show thisYear))
      ]
      output
  Route.rule Route.essays $ \() output -> do
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
  Route.rule Route.essay $ \input output -> do
    need [input]
    env <- envCache
    templates <- templateCache
    (v, content) <- getMd input
    essayMetadata :: Essay <- interpretValue v
    renderAndWrite
      templates
      ["essay", "default"]
      (Just content)
      [ menuItem Essays env,
        toJSON essayMetadata
      ]
      output
  Route.rule Route.contact $ \input output -> do
    need [input]
    env <- envCache
    templates <- templateCache
    (v, content) <- getMd input
    renderAndWrite
      templates
      ["default"]
      (Just content)
      [ menuItem Contact env,
        mkTitle Contact,
        v
      ]
      output

----------------------------------------------------------------------------
-- Helpers

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

mkContext :: [Value] -> Value
mkContext = foldl1' f
  where
    f (Object m0) (Object m1) = Object (KeyMap.union m0 m1)
    f _ _ = error "context merge failed"

mkTitle :: MenuItem -> Value
mkTitle = provideAs "title" . menuItemTitle

provideAs :: (ToJSON v) => Text -> v -> Value
provideAs k v = Object (KeyMap.singleton (Key.fromText k) (toJSON v))

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
  Markdown.render env txt path

interpretValue :: (FromJSON v) => Value -> Action v
interpretValue v =
  case fromJSON v of
    Error str -> fail str
    Success a -> return a

exhibitionYear :: Exhibition -> Year
exhibitionYear Exhibition {..} = dayPeriod exhibitionStart

getArtworkYears :: [Artwork] -> Set Year
getArtworkYears = Set.fromList . fmap (dayPeriod . Artwork.date)

filterArtworksByYear :: Year -> [Artwork] -> [Artwork]
filterArtworksByYear year = filter f
  where
    f x = dayPeriod (Artwork.date x) == year
