{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Yesod.Markdown

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Yesod.Markdown" $ do
    it "converts Markdown to sanitized HTML" $ do
        let
            markdown = Markdown $ T.unlines
                [ "# Title"
                , ""
                , "- one"
                , "- two"
                , "- three"
                , ""
                , "<script>"
                , "  alert('xxs');"
                , "</script>"
                ]

        let Right html = markdownToHtml markdown

        -- Trim tag linebreaks due to non-functional difference in newer version
        -- of xss or pandoc library.
        let rendered = TL.replace ">\n<" "><" $ renderHtml html
            expectedPrefix = mconcat
                [ "<h1 id=\"title\">Title</h1>"
                , "<ul>"
                , "<li>one</li>"
                , "<li>two</li>"
                , "<li>three</li>"
                , "</ul>"
                ]

        rendered `shouldSatisfy` (expectedPrefix `TL.isPrefixOf`)
        rendered `shouldSatisfy` (not . ("<script>" `TL.isInfixOf`))

    it "converts Markdown to unsanitized HTML" $ do
        let
            markdown = Markdown $ T.unlines
                [ "# Title"
                , ""
                , "- one"
                , "- two"
                , "- three"
                , ""
                , "<script>"
                , "  alert('xxs');"
                , "</script>"
                ]

        let Right html = markdownToHtmlTrusted markdown

        TL.replace ">\n<" "><" (renderHtml html) `shouldBe` TL.concat
            [ "<h1 id=\"title\">Title</h1>"
            , "<ul>"
            , "<li>one</li>"
            , "<li>two</li>"
            , "<li>three</li>"
            , "</ul>"
            , "<script>\n"
            , "  alert('xxs');\n"
            , "</script>"
            ]
