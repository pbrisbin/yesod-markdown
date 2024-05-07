# Yesod Markdown

[![Hackage](https://img.shields.io/hackage/v/yesod-markdown.svg?style=flat)](https://hackage.haskell.org/package/yesod-markdown)
[![Stackage Nightly](http://stackage.org/package/yesod-markdown/badge/nightly)](http://stackage.org/nightly/package/yesod-markdown)
[![Stackage LTS](http://stackage.org/package/yesod-markdown/badge/lts)](http://stackage.org/lts/package/shellwords)
[![CI](https://github.com/pbrisbin/yesod-markdown/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/yesod-markdown/actions/workflows/ci.yml)

A small wrapper over [Pandoc][]'s powerful `Markdown -> Html` support, with
usage tailored for Yesod.

[pandoc]: http://hackage.haskell.org/package/pandoc

## Usage

```hs
getPageR :: FilePath -> Handler RepHtml
getPageR fp = do
    content <- liftIO
        $ fmap markdownToHtml
        $ markdownFromFile fp

    defaultLayout $ do
        [shamlet|
            <div class="content">
                #{content}
            |]
```

The default extensions are minimal, you can specify your preferred
[extensions][] with `markdownToHtmlWithExtensions`:

[extensions]: http://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Extensions.html

```haskell
import Text.Pandoc.Extensions (githubMarkdownExtensions)

getPageR :: FilePath -> Handler RepHtml
getPageR fp = do
    content <- liftIO
        $ fmap (markdownToHtmlWithExtensions githubMarkdownExtensions)
        $ markdownFromFile fp

    defaultLayout $ do
        [shamlet|
            <div class="content">
                #{content}
            |]
```

For more information, see the [haddocks][].

[haddocks]: http://hackage.haskell.org/package/yesod-markdown/docs/Yesod-Markdown.html

## Developing & Tests

```
stack setup
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
