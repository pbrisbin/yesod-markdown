{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Markdown
  ( Markdown(..)
  -- * Wrappers
  , markdownToHtml
  , markdownToHtmlTrusted
  , markdownFromFile
  -- * Conversions
  , parseMarkdown
  , writePandoc
  , writePandocTrusted
  -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultReaderOptions
  -- * Form helper
  , markdownField
  )
  where

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
import Text.Pandoc
import Yesod.Core (HandlerSite, RenderMessage)
import Yesod.Core.Widget (toWidget)
import Yesod.Form.Functions (parseHelper)
import Yesod.Form.Types

import qualified Data.ByteString as B
import qualified Data.Text as T

newtype Markdown = Markdown { unMarkdown :: Text }
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

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

markdownToHtml :: Markdown -> Either PandocError Html
markdownToHtml = fmap (writePandoc yesodDefaultWriterOptions)
    . parseMarkdown yesodDefaultReaderOptions

-- | No HTML sanitization
markdownToHtmlTrusted :: Markdown -> Either PandocError Html
markdownToHtmlTrusted = fmap (writePandocTrusted yesodDefaultWriterOptions)
    . parseMarkdown yesodDefaultReaderOptions

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

writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedToMarkup . sanitizeBalance . T.pack . writeHtmlString wo

writePandocTrusted :: WriterOptions -> Pandoc -> Html
writePandocTrusted wo = preEscapedToMarkup . writeHtmlString wo

parseMarkdown :: ReaderOptions -> Markdown -> Either PandocError Pandoc
parseMarkdown ro = readMarkdown ro . T.unpack . unMarkdown

-- | Defaults plus Html5, minus WrapText
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = def
  { writerHtml5 = True
  , writerWrapText  = WrapNone
  , writerHighlight = True
  }

-- | Defaults plus Smart and ParseRaw
yesodDefaultReaderOptions :: ReaderOptions
yesodDefaultReaderOptions = def
    { readerSmart = True
    , readerParseRaw = True
    }
