{-# LANGUAGE OverloadedStrings #-}
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

        let rendered = renderHtml html
        rendered `shouldBe` TL.concat
            [ "<h1 id=\"title\">Title</h1>"
            , "<ul>"
            , "<li>one</li>"
            , "<li>two</li>"
            , "<li>three</li>"
            , "</ul>"
            -- pandoc-2.2.3 changes quote escaping in a minor way that we don't
            -- care about. CPP is annoying, so let's just make the assertion
            -- dynamic
            , if "&#39;" `TL.isInfixOf` rendered
                then "\n  alert(&#39;xxs&#39;);\n"
                else "\n  alert('xxs');\n"
            ]

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

        renderHtml html `shouldBe` TL.concat
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
