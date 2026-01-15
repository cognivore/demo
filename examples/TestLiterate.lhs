This is a literate Haskell test file.

> {-# LANGUAGE OverloadedStrings #-}
> module TestLiterate (presentation) where
>
> import Demo.Core.DSL
>
> presentation :: Presentation
> presentation = mkPresentation "Literate Test" $ do
>   slide "Hello Literate" $ do
>     note "This is a literate Haskell presentation!"
>     system "echo 'Hello from literate Haskell!'"
