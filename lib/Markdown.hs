{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Markdown
  ( render,
  )
where

import Control.Exception (ErrorCall (..), throwIO)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Lens
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Development.Shake hiding (Verbosity (..))
import Lucid qualified as L
import Text.MMark (MMark)
import Text.MMark qualified as MMark
import Text.MMark.Extension.Common qualified as Ext
import Text.MMark.Render (RenderExtension)
import Text.MMark.Render qualified as Render
import Text.MMark.Trans (Block (..), Bni, Inline (..), Trans, TransError)
import Text.MMark.Trans qualified as Trans
import Text.Megaparsec qualified as M
import Text.URI (URI)
import Text.URI qualified as URI
import Text.URI.Lens (uriPath, uriScheme)
import Text.URI.QQ (scheme)

-- | Render a markdown document.
render :: Value -> Text -> FilePath -> Action (Value, TL.Text)
render env txt path =
  case MMark.parse path txt of
    Left bundle -> reportDiagnostics bundle
    Right doc -> do
      let headings = MMark.runScanner Ext.headingScanner doc
          headerIds = MMark.runScanner Ext.headerIdScanner doc
          footnotes = MMark.runScanner Ext.footnoteScanner doc
          toc = MMark.runScanner (Ext.tocScanner (\x -> x > 1 && x < 4)) doc
          transformations =
            Ext.checkFragments (headerIds <> footnoteIds doc)
              >=> Ext.checkAltText
              >=> Ext.punctuationPrettifier
              >=> Ext.toc "toc" toc
              >=> provideSocialUrls env
      orFail (MMark.runCheck (Ext.checkHeadings headings) doc)
      orFail (MMark.runCheck (Ext.validateFootnotes footnotes) doc)
      doc' <- orFail (MMark.runTrans transformations doc)
      let v = fromMaybe (object []) (MMark.projectYaml doc')
      return (v, L.renderText (MMark.render renderExtensions doc'))
  where
    orFail :: Either (M.ParseErrorBundle Text TransError) a -> Action a
    orFail = either reportDiagnostics return

-- | Fail the build with diagnostics megaparsec has already laid out for a
-- terminal.
--
-- Not 'fail': that builds the error with 'userError', which shows itself as
-- @user error (…)@, and since the diagnostics end with a newline of their own
-- the closing parenthesis lands on a line by itself under them. An 'ErrorCall'
-- shows itself as nothing but its message, which is what these already are.
reportDiagnostics ::
  (M.ShowErrorComponent e, M.VisualStream s, M.TraversableStream s) =>
  M.ParseErrorBundle s e ->
  Action a
reportDiagnostics =
  liftIO . throwIO . ErrorCall . dropWhileEnd isSpace . M.errorBundlePretty

-- | The ids that the footnote section of a document defines, that is @fn1@,
-- @fn2@, and so on.
--
-- 'Ext.headerIdScanner' only knows about headings, so without these a
-- hand-written link into the footnote section would look to
-- 'Ext.checkFragments' like a link that leads nowhere. Such a link is the only
-- way to point at a footnote a second time, since @footnote:n@ may be used
-- once per footnote and no more (every reference carries an id of its own).
--
-- The shape recognized here is the one 'Ext.footnotes' renders: a blockquote
-- holding the word @footnotes@ and then the ordered list of the footnotes,
-- which is numbered from wherever the list starts.
footnoteIds :: MMark -> Set Text
footnoteIds = MMark.runScanner (MMark.scanner Set.empty add)
  where
    add acc = \case
      Blockquote _ [Paragraph _ label, OrderedList _ i items]
        | Trans.asPlainText label == "footnotes" ->
            foldr (Set.insert . footnoteId) acc (take (length items) [i ..])
      _ -> acc
    footnoteId :: Word -> Text
    footnoteId n = "fn" <> T.pack (show n)

-- | Everything that shapes the HTML of a document.
--
-- The order matters where two extensions look at the same inline: the one
-- that comes first is entered first and sees the markup as it was written,
-- and what it delegates to is the rest of the list. 'addLinkRel' has to come
-- before 'Ext.linkTarget' for that reason, see its note.
renderExtensions :: RenderExtension
renderExtensions =
  mconcat
    [ Ext.footnotes,
      Ext.kbd,
      addLinkRel,
      Ext.linkTarget,
      Ext.permalinksWith (\n -> n >= 2 && n <= 4) "anchor" Nothing anchorIcon,
      Ext.lazyImages,
      addTableClasses,
      addImageClasses
    ]

addTableClasses :: RenderExtension
addTableClasses = Render.blockRender $ \old block ->
  case block of
    t@Table {} -> L.with (old t) [L.class_ "site-table"]
    other -> old other

addImageClasses :: RenderExtension
addImageClasses = Render.inlineRender $ \old inline ->
  case inline of
    i@Image {} -> L.with (old i) [L.class_ "site-image"]
    other -> old other

-- | Add @rel="noopener noreferrer"@ to external links (those with an http/https
-- scheme). Prevents reverse-tabnabbing on @target="_blank"@ links and follows
-- current best practice for cross-origin links generally.
--
-- 'Ext.linkTarget' gives a link that opens in a new browsing context that
-- same rel of its own, so this steps aside for those. Lucid does not drop a
-- repeated attribute or keep it twice, it runs the two values together into
-- a single @rel="noopener noreferrernoopener noreferrer"@, which names no
-- relationship at all. This has to be entered before 'Ext.linkTarget' to be
-- able to tell: by the time 'Ext.linkTarget' delegates, it has already
-- taken the target out of the title and there is nothing left here to
-- recognize.
addLinkRel :: RenderExtension
addLinkRel = Render.inlineRender $ \old inline ->
  case inline of
    l@(Link _ _ uri mtitle)
      | isExternal uri,
        not (opensNewContext mtitle) ->
          L.with (old l) [L.rel_ "noopener noreferrer"]
    other -> old other
  where
    isExternal uri = case URI.uriScheme uri of
      Just s -> s == [scheme|http|] || s == [scheme|https|]
      Nothing -> False
    opensNewContext = maybe False ("_blank" `T.isPrefixOf`)

-- | The little link glyph shown next to headings.
anchorIcon :: L.Html ()
anchorIcon =
  L.toHtmlRaw
    ( "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" "
        <> "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
        <> "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
        <> "class=\"inline-block align-[-0.125em]\">"
        <> "<path d=\"M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71\"></path>"
        <> "<path d=\"M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71\"></path>"
        <> "</svg>" ::
        Text
    )

provideSocialUrls :: Value -> Bni -> Trans Bni
provideSocialUrls v = Trans.bottomUpInlines $ \case
  l@(Link spn inner uri mtitle)
    | URI.uriScheme uri == Just [scheme|social|] ->
        case uri ^. uriPath of
          [x] ->
            let field = URI.unRText x
                social l' = v ^? key "social" . key (Key.fromText field) . l'
                -- The email field is a bare address in env.yaml, so it needs a
                -- mailto: scheme; every other social field is already a URL.
                withScheme = if field == "email" then uriScheme ?~ [scheme|mailto|] else id
             in case (,) <$> social _String <*> social (_String . getURI) of
                  Nothing -> do
                    Trans.report
                      spn
                      ("env.yaml has no social account called \"" <> field <> "\"")
                    return l
                  Just (raw, t) ->
                    return $
                      if Trans.asPlainText inner == "x"
                        then
                          -- The "x" sentinel means: show the raw social value
                          -- as the link text (rendering the URI would
                          -- percent-encode it, e.g. turning "@" into "%40").
                          Link spn (Plain spn raw :| []) (withScheme t) mtitle
                        else Link spn inner (withScheme t) mtitle
          _ -> return l
  other -> return other

getURI :: Traversal' Text URI
getURI f txt = maybe txt URI.render <$> traverse f (URI.mkURI txt :: Maybe URI)
