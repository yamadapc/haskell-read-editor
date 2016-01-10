haskell-read-editor
===================
[![Build Status](https://travis-ci.org/yamadapc/haskell-read-editor.svg?branch=master)](https://travis-ci.org/yamadapc/haskell-read-editor)
- - -
Opens a temporary file on the system's EDITOR and returns the resulting edits

Published to hackage as [`read-editor`](https://hackage.haskell.org/package/read-editor).

## Usage
```haskell
module Main where

import System.ReadEditor

main :: IO ()
main = do
    putStrLn ">>> Opening editor for you to write me some content"
    content <- readEditor
    putStrLn ">>> You wrote:"
    putStrLn content
```

### Reading edits of existing content
```haskell
module Main where

import System.ReadEditor

main :: IO ()
main = do
    putStrLn ">>> Opening editor for you to write me some content"
    content <- readEditorWith "Some stuff that's already here"
    putStrLn ">>> You wrote:"
    putStrLn content
```

## Support through CI
Travis-CI is building this package with GHC versions 7.6, 7.8 and 7.10. If the
build status is green, they should all be supported. Check the [project page](https://travis-ci.org/yamadapc/haskell-read-editor)
to see more.

## License
This code is licensed under the MIT license for Pedro Tacla Yamada. For more
information please refer to the [LICENSE](/LICENSE) file.
