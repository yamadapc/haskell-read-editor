module System.ReadEditorSpec
  where

import           Test.Hspec

spec :: Spec
spec = do
    describe "readEditor :: IO String" $
        describe "given a mock environment" $
            it "runs our mock process" pending

    describe "openEditor :: FilePath -> IO ()" $
        describe "given a mock environment" $
            it "runs our mock process" pending
