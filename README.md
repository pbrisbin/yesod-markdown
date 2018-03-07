# Yesod Markdown

A small wrapper over [Pandoc][]'s powerful `Markdown -> Html` support, with
usage tailored for Yesod.

[pandoc]: http://hackage.haskell.org/package/pandoc

## Usage

```haskell
getPageR :: FilePath -> Handler RepHtml
getPageR fp = do
    content <- liftIO $ fmap markdownToHtml (markdownFromFile fp)

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
