{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Route
  ( Route,
    art,
    artYear,
    contact,
    essay,
    essays,
    exhibition,
    exhibitions,
    notFound,
    rule,
    page,
    essayPattern,
    WithPage,
    withPage,
    unWithPage,
  )
where

import Crypto.Hash qualified as Hash
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time
import Development.Shake
import Development.Shake.FilePath
import Utils (getMatchingFiles, newCache')

-- | A route on the site that can be built. The type variable @i@ is either
-- the input file path (for pages that are generated from an input file) or
-- a characteristic id that varies depending on how the resulting pages are
-- individualized.
data Route i where
  -- | A page that is produced from a corresponding input file.
  FromInput :: FilePattern -> (FilePath -> FilePath) -> Route FilePath
  -- | Generate a fixed file.
  Fixed :: FilePath -> Route ()
  -- | Generate an open set of outputs. The second argument maps a
  -- characteristic identifier to the resulting output file. The third
  -- argument specifies how to yield the full range of characteristic
  -- identifiers.
  OpenSet :: FilePattern -> (i -> FilePath) -> Action (Set i) -> Route i

art, essays, exhibitions, notFound :: Route ()
art = Fixed "art.html"
essays = Fixed "essays.html"
exhibitions = Fixed "exhibitions.html"
notFound = Fixed "404.html"

contact, essay :: Route FilePath
contact = FromInput "contact.md" (-<.> "html")
essay = FromInput essayPattern (-<.> "html")

artYear :: Action (Set Year) -> Route Year
artYear =
  OpenSet
    "art/*.html"
    (\year -> "art" </> show year <.> "html")

exhibition :: Action (Set (Hash.Digest SHA256)) -> Route (Hash.Digest SHA256)
exhibition =
  OpenSet
    "exhibition/*.html"
    (\digest -> "exhibition" </> show digest <.> "html")

-- | Take an input file or a characteristic identifier (in the case of an
-- open set) and map it to the output page location.
page :: Route i -> i -> FilePath
page route i = "/" ++ outputWithoutOutdir route i

-- | Similar to 'outputWithoutOutdir', but prefixes it with 'outdir'.
outputWithOutdir :: Route i -> i -> FilePath
outputWithOutdir route i = outdir </> outputWithoutOutdir route i

-- | Render the location of a route without prefixing it with 'outdir'.
outputWithoutOutdir :: Route i -> i -> FilePath
outputWithoutOutdir route i = mapOut i
  where
    mapOut = case route of
      FromInput _pattern f -> f
      Fixed path -> const path
      OpenSet _pattern f _ -> f

-- | A helper for defining rules.
rule ::
  -- | 'Route' we want to build
  Route i ->
  -- | Input file or a characteristic id, output file
  (i -> FilePath -> Action ()) ->
  Rules ()
rule route@(FromInput pat _) f = do
  action $
    getMatchingFiles pat >>= need . fmap (outputWithOutdir route)
  inputMap <- newCache' $ \() -> do
    ifiles <- getMatchingFiles pat
    return $ Map.fromList (zip (outputWithOutdir route <$> ifiles) ifiles)
  outputWithOutdir route pat %> \output -> do
    input <- (Map.! output) <$> inputMap
    f input output
rule route@(Fixed _) f = do
  let output' = outputWithOutdir route ()
  want [output']
  output' %> \output ->
    f () output
rule route@(OpenSet pat _ yieldIds) f = do
  idMap <- newCache' $ \() -> do
    allIds <- Set.toAscList <$> yieldIds
    let allOuts = outputWithOutdir route <$> allIds
    return $ Map.fromList (zip allOuts allIds)
  action $
    idMap >>= need . Map.keys
  let output' = outdir </> pat
  output' %> \output -> do
    characteristicId <- (Map.! output) <$> idMap
    f characteristicId output

-- | Top-level output directory of the site.
outdir :: FilePath
outdir = "_build"

essayPattern :: FilePath
essayPattern = "essay/*.md"

-- | This wrapper adds the field called @"page"@ to the JSON object that the
-- inner type generates.
data WithPage a = WithPage FilePath a

instance (ToJSON a) => ToJSON (WithPage a) where
  toJSON (WithPage page' a) =
    case toJSON a of
      Object keyMap ->
        Object (KeyMap.insert "page" (toJSON page') keyMap)
      somethingElse ->
        object
          [ "page" .= page',
            "value" .= somethingElse
          ]

-- | Wrap a data type with 'WithPage'.
withPage :: Route i -> i -> a -> WithPage a
withPage route inputOrId x =
  WithPage (page route inputOrId) x

-- | Unwrap 'WithPage'.
unWithPage :: WithPage a -> a
unWithPage (WithPage _ x) = x
