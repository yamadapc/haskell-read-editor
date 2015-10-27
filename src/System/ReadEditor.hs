module System.ReadEditor
       ( -- * Main exports
         readEditor
       , openEditor
       , openEditorWithPrompt
         -- * Utility
       , prompt
       , withSystemTempFile
       )
  where

import           Control.Exception  (finally)
import           System.Directory   (getTemporaryDirectory, removeFile)
import           System.Environment (getEnv)
import           System.IO          (Handle, hClose, hFlush, openTempFile,
                                     stdout)
import           System.IO.Error    (catchIOError)
import           System.Process     (system)

-- | Opens a file in the sytem's editor and returns it's contents after it's saved.
readEditor :: IO String
readEditor = withSystemTempFile "read-editor" $ \fp _ -> do
    openEditor fp
    readFile fp

-- | Opens a file in the sytem's editor, waits until it's closed.
openEditor :: FilePath -> IO ()
openEditor = openEditorWithPrompt "What editor should I use? "

-- | A version of 'openEditor' which takes the fallback prompt question.
openEditorWithPrompt :: String -> FilePath -> IO ()
openEditorWithPrompt p fp = do
    editorCmd <- catchIOError (getEnv "EDITOR") (const promptForEditor)
    _ <- system $ editorCmd ++ " " ++ fp
    return ()
  where promptForEditor = prompt p

-- | Prompts for a line and returns it
prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine

-- | Executes a function with an open temporary file Handle.
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile templ fn = do
    tempdir <- catchIOError getTemporaryDirectory (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir templ
    finally (fn tempfile temph) (hClose temph >> removeFile tempfile)
