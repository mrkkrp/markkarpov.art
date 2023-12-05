{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
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
    exhibitionArtworks :: Set Artwork
  }
  deriving (Eq, Show)

instance FromJSON Exhibition where
  parseJSON = undefined

instance ToJSON Exhibition where
  toJSON = undefined

-- | Information about an artwork.
data Artwork = Artwork
  { artworkTitle :: !Text,
    artworkDescription :: !(Maybe Text),
    artworkMedium :: !Medium,
    artworkHeight :: !Int,
    artwokrWidth :: !Int,
    artworkDate :: !Day
  }
  deriving (Eq, Show)

instance FromJSON Artwork where
  parseJSON = undefined

instance ToJSON Artwork where
  toJSON = undefined

-- | Art mediums.
data Medium
  = OilOnCanvas
  deriving (Eq, Show)

instance FromJSON Medium where
  parseJSON = undefined

instance ToJSON Medium where
  toJSON = undefined

-- | Information about an essay.
data Essay = Essay
  { essayTitle :: !Text,
    essayPublished :: !Day,
    essayUpdated :: !(Maybe Day),
    essayFile :: !FilePath
  }
  deriving (Eq, Show)

instance FromJSON Essay where
  parseJSON = undefined

instance ToJSON Essay where
  toJSON = undefined

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
