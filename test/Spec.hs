import           Control.Exception (evaluate)
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "test errorchecking 1" $ do
    it "throws Invalid type error 1" $ do
      res <- Lib.test ["(+ 2 \"two\")"]
      res `shouldBe` "Invalid type: expected number, found \"two\""
    it "throws Invalid type error 2" $ do
      res <- Lib.test ["(+ 2)"]
      res `shouldBe` "Expected 2 args; found values 2"
    it "throws Invalid type error 3" $ do
      res <- Lib.test["(what? 2)"]
      res  `shouldBe` "Unrecognized primitive function args: \"what?\""
