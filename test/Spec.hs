import           Control.Exception (evaluate)
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "test eval 1" $ do
    it "evals atom" $ do
      res <- Lib.test ["'atom"]
      res `shouldBe` "atom"
    it "evals string" $ do
      res <- Lib.test ["\"a string\""]
      res `shouldBe` "\"a string\""
    it "evals simple numericBinop" $ do
      res <- Lib.test["(+ 2 2)"]
      res  `shouldBe` "4"
    it "evals compound numericBinop" $ do
      res <- Lib.test["(+ 2 (+ 3 2))"]
      res  `shouldBe` "7"
  describe "test errorchecking 1" $ do
    it "throws Invalid type error 1" $ do
      res <- Lib.test ["(+ 2 \"two\")"]
      res `shouldBe` "Invalid type: expected number, found \"two\""
    it "throws arg error" $ do
      res <- Lib.test ["(+ 2)"]
      res `shouldBe` "Expected 2 args; found values 2"
    it "throws unrecognized fn error" $ do
      res <- Lib.test["(what? 2)"]
      res  `shouldBe` "Unrecognized primitive function args: \"what?\""
  describe "test eval 2" $ do
    it "evals boolBinop" $ do
      res <- Lib.test ["(< 2 3)"]
      res `shouldBe` "#t"
    it "evals boolBinop 2" $ do
      res <- Lib.test ["(< 3 2)"]
      res `shouldBe` "#f"
    it "evals strBoolBinop" $ do
      res <- Lib.test ["(string=? \"test\"  \"test\")"]
      res `shouldBe` "#t"
    it "handles conditionals" $ do
      res <- Lib.test ["(if (> 2 3) \"no\" \"yes\")"]
      res `shouldBe` "\"yes\""
