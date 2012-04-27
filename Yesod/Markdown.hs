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
  , yesodDefaultParserState
  -- * Form helper
  , markdownField
  )
  where

import Yesod.Form (ToField(..), areq, aopt)
import Yesod.Core (RenderMessage, SomeMessage(..))
import Yesod.Form.Types
import Yesod.Widget (toWidget)
import Text.Hamlet (hamlet, Html)
import Database.Persist (PersistField)

import Text.Blaze.Html (toHtml)
import Text.Pandoc
import Text.HTML.SanitizeXSS (sanitizeBalance)

import Data.Monoid      (Monoid)
import Data.String      (IsString)
import System.Directory (doesFileExist)

import qualified Data.Text as T

newtype Markdown = Markdown String
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance ToField Markdown master where
    toField = areq markdownField

instance ToField (Maybe Markdown) master where
    toField = aopt markdownField

markdownField :: RenderMessage master FormMessage => Field sub master Markdown
markdownField = Field
    { fieldParse = blank $ Right . Markdown . unlines . lines' . T.unpack
    , fieldView  = \theId name attrs val _isReq -> toWidget
        [hamlet|
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
|]
     }

     where
        unMarkdown :: Markdown -> T.Text
        unMarkdown (Markdown s) = T.pack s

        lines' :: String -> [String]
        lines' = map go . lines

        go []        = []
        go ('\r':xs) = go xs
        go (x:xs)    = x : go xs

blank :: (Monad m, RenderMessage master FormMessage)
      => (T.Text -> Either FormMessage a)
      -> [T.Text]
      -> m (Either (SomeMessage master) (Maybe a))
blank _ []     = return $ Right Nothing
blank _ ("":_) = return $ Right Nothing
blank f (x:_)  = return $ either (Left . SomeMessage) (Right . Just) $ f x

-- | Converts markdown directly to html using the yesod default option 
--   sets and sanitization.
markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions
               . parseMarkdown yesodDefaultParserState

-- | Same but with no sanitization run
markdownToHtmlTrusted :: Markdown -> Html
markdownToHtmlTrusted = writePandocTrusted yesodDefaultWriterOptions
                      . parseMarkdown yesodDefaultParserState

-- | Reads markdown in from the specified file; returns the empty string 
--   if the file does not exist
markdownFromFile :: FilePath -> IO Markdown
markdownFromFile f = do
    exists <- doesFileExist f
    content <- do
        if exists
            then readFile f
            else return ""

    return $ Markdown content

-- | Converts the intermediate Pandoc type to Html. Sanitizes HTML.
writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = toHtml . sanitizeBalance . T.pack . writeHtmlString wo

-- | Skips the sanitization and its required conversion to Text
writePandocTrusted :: WriterOptions -> Pandoc -> Html
writePandocTrusted wo = toHtml . writeHtmlString wo

-- | Parses Markdown into the intermediate Pandoc type
parseMarkdown :: ParserState -> Markdown -> Pandoc
parseMarkdown ro (Markdown m) = readMarkdown ro m

-- | Pandoc defaults, plus Html5, minus WrapText
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = defaultWriterOptions
  { writerHtml5    = True
  , writerWrapText = False
  }

-- | Pandoc defaults, plus Smart, plus ParseRaw
yesodDefaultParserState :: ParserState
yesodDefaultParserState = defaultParserState
    { stateSmart    = True
    , stateParseRaw = True
    }
