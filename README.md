# Yesod Markdown

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

The default extensions are minimal, you can specify you're preferred
[extensions][] with `markdownToHtmlWithExtensions`:

[extensions](http://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Extensions.html)

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
