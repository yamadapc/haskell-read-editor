haskell-read-editor
===================
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

## License
This code is licensed under the MIT license for Pedro Tacla Yamada. For more
information please refer to the [LICENSE](/LICENSE) file.
