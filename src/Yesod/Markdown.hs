{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Yesod.Markdown
  ( Markdown(..)
  -- * Wrappers
  , markdownToHtml
  , markdownToHtmlTrusted
  , markdownToHtmlWithExtensions
  , markdownToHtmlWith
  , markdownFromFile
  -- * Conversions
  , parseMarkdown
  , writePandoc
  , writePandocTrusted
  -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultReaderOptions
  , yesodDefaultExtensions
  -- * Form helper
  , markdownField
  ) where

import Control.Monad ((<=<))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Database.Persist (PersistField, SqlType(SqlString))
import Database.Persist.Sql (PersistFieldSql(..))
import System.Directory (doesFileExist)
import Text.Blaze (ToMarkup(toMarkup))
import Text.Blaze.Html (preEscapedToMarkup)
import Text.Hamlet (Html, hamlet)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Pandoc hiding (handleError)
import Yesod.Core (HandlerSite, RenderMessage)
import Yesod.Core.Widget (toWidget)
import Yesod.Form.Functions (parseHelper)
import Yesod.Form.Types

import qualified Data.ByteString as B
import qualified Data.Text as T

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

newtype Markdown = Markdown { unMarkdown :: Text }
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid, Semigroup)

instance PersistFieldSql Markdown where
    sqlType _ = SqlString

instance ToMarkup Markdown where
    -- | Sanitized by default
    toMarkup = handleError . markdownToHtml

markdownField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . T.filter (/= '\r')
    , fieldView  = \theId name attrs val _isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
|]
    , fieldEnctype = UrlEncoded
    }

-- | Process Markdown using our options and sanitization
markdownToHtml :: Markdown -> Either PandocError Html
markdownToHtml = markdownToHtmlWith
    yesodDefaultReaderOptions
    yesodDefaultWriterOptions

-- | No HTML sanitization
--
-- **NOTE**: Use only with /fully-trusted/ input.
--
markdownToHtmlTrusted :: Markdown -> Either PandocError Html
markdownToHtmlTrusted = markdownToHtmlWith' id
    yesodDefaultReaderOptions
    yesodDefaultWriterOptions

-- | Process markdown with given extensions
--
-- Uses our options, and overrides extensions only.
--
-- > markdownToHtmlWithExtensions githubMarkdownExtensions
--
markdownToHtmlWithExtensions
    :: Extensions
    -> Markdown
    -> Either PandocError Html
markdownToHtmlWithExtensions exts = markdownToHtmlWith
    yesodDefaultReaderOptions { readerExtensions = exts }
    yesodDefaultWriterOptions { writerExtensions = exts }

-- | Fully controllable Markdown processing
markdownToHtmlWith
    :: ReaderOptions
    -> WriterOptions
    -> Markdown
    -> Either PandocError Html
markdownToHtmlWith = markdownToHtmlWith' sanitizeBalance

-- | Internal function, the only way to skip sanitization
markdownToHtmlWith'
    :: (Text -> Text)
    -> ReaderOptions
    -> WriterOptions
    -> Markdown
    -> Either PandocError Html
markdownToHtmlWith' sanitize ropts wopts =
    writePandocWith sanitize wopts <=< parseMarkdown ropts

-- | Returns the empty string if the file does not exist
markdownFromFile :: FilePath -> IO Markdown
markdownFromFile f = do
    exists <- doesFileExist f
    Markdown <$> if exists
        then readFileUtf8 f
        else return ""

  where
    readFileUtf8 :: FilePath -> IO Text
    readFileUtf8 fp = decodeUtf8With lenientDecode <$> B.readFile fp

writePandoc :: WriterOptions -> Pandoc -> Either PandocError Html
writePandoc = writePandocWith sanitizeBalance
{-# DEPRECATED writePandoc "Don't use this directly" #-}

writePandocTrusted :: WriterOptions -> Pandoc -> Either PandocError Html
writePandocTrusted = writePandocWith id
{-# DEPRECATED writePandocTrusted "Don't use this directly" #-}

writePandocWith
    :: (Text -> Text)
    -> WriterOptions
    -> Pandoc
    -> Either PandocError Html
writePandocWith f wo
    = (preEscapedToMarkup . f <$>)
    . runPure
    . writeHtml5String wo

parseMarkdown :: ReaderOptions -> Markdown -> Either PandocError Pandoc
parseMarkdown ro = runPure . readMarkdown ro . unMarkdown
{-# DEPRECATED parseMarkdown "Don't use this directly" #-}

-- | Defaults minus WrapText, plus our extensions
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = def
    { writerWrapText = WrapNone
    , writerExtensions = extensionsFromList yesodDefaultExtensions
    }

-- | Defaults plus our extensions, see @'yesodDefaultExtensions'@
yesodDefaultReaderOptions :: ReaderOptions
yesodDefaultReaderOptions = def
    { readerExtensions = extensionsFromList yesodDefaultExtensions
    }

-- | @raw_html@ and @auto_identifiers@
yesodDefaultExtensions :: [Extension]
yesodDefaultExtensions =
    [ Ext_raw_html
    , Ext_auto_identifiers
    ]

-- | Unsafely handle a @'PandocError'@
--
-- This is analagous to pandoc-1 behavior, and is required in a pure context
-- such as the @'ToMarkup'@ instance.
--
handleError :: Either PandocError a -> a
handleError = either (error . show) id
