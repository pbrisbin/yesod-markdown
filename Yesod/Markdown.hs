{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
--
-- Rewrite/simplification of yesod-markdown written by ajdunlap.
--
-- Forked from <https://github.com/ajdunlap/yesod-markdown>.
--
-------------------------------------------------------------------------------
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

import Data.Monoid (Monoid)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Database.Persist (PersistField)
import System.Directory (doesFileExist)

import Text.Blaze (ToMarkup (toMarkup))
import Text.Blaze.Html (preEscapedToMarkup)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet (hamlet, Html)
import Text.Pandoc

import Yesod.Core (RenderMessage)
import Yesod.Form (ToField(..), areq, aopt)
import Yesod.Form.Functions (parseHelper)
import Yesod.Form.Types
import Yesod.Core.Widget (toWidget)

import qualified Data.ByteString as B
import qualified Data.Text       as T

newtype Markdown = Markdown { unMarkdown :: Text }
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance ToMarkup Markdown where
    -- | Sanitized by default
    toMarkup = markdownToHtml

instance ToField Markdown master where
    toField = areq markdownField

instance ToField (Maybe Markdown) master where
    toField = aopt markdownField

markdownField :: RenderMessage master FormMessage => Field sub master Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . T.filter (/= '\r')
    , fieldView  = \theId name attrs val _isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
|]
    , fieldEnctype = UrlEncoded
    }

markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions
               . parseMarkdown yesodDefaultReaderOptions

-- | No HTML sanitization
markdownToHtmlTrusted :: Markdown -> Html
markdownToHtmlTrusted = writePandocTrusted yesodDefaultWriterOptions
                      . parseMarkdown yesodDefaultReaderOptions

-- | Returns the empty string if the file does not exist
markdownFromFile :: FilePath -> IO Markdown
markdownFromFile f = do
    exists  <- doesFileExist f
    content <-
        if exists
            then readFileUtf8 f
            else return ""

    return $ Markdown content

    where
        readFileUtf8 :: FilePath -> IO Text
        readFileUtf8 fp = do
            bs <- B.readFile fp
            return $ decodeUtf8With lenientDecode bs

writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedToMarkup . sanitizeBalance . T.pack . writeHtmlString wo

writePandocTrusted :: WriterOptions -> Pandoc -> Html
writePandocTrusted wo = preEscapedToMarkup . writeHtmlString wo

parseMarkdown :: ReaderOptions -> Markdown -> Pandoc
parseMarkdown ro = readMarkdown ro . T.unpack . unMarkdown

-- | Defaults plus Html5, minus WrapText
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = def
  { writerHtml5    = True
  , writerWrapText = False
  }

-- | Defaults plus Smart and ParseRaw
yesodDefaultReaderOptions :: ReaderOptions
yesodDefaultReaderOptions = def
    { readerSmart    = True
    , readerParseRaw = True
    }
