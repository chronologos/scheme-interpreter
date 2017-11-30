import           Control.Exception (evaluate)
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "test eval 1" $ do
    it "evals Atom" $ do
      res <- Lib.test ["'atom"]
      res `shouldBe` "atom"
    it "evals quoted Atom" $ do
      res <- Lib.test ["'(atom)"]
      res `shouldBe` "(atom)"
    it "evals String" $ do
      res <- Lib.test ["\"a string\""]
      res `shouldBe` "\"a string\""
    it "evals Bool (atom)" $ do
      res <- Lib.test ["#t"]
      res `shouldBe` "#t"
    it "evals DottedList" $ do
      res <- Lib.test ["'(1 2 . 1)"]
      res `shouldBe` "(1 2 . 1)"
    it "evals DottedList (quoted)" $ do
      res <- Lib.test ["(quote ((1 2) . 1))"]
      res `shouldBe` "((1 2) . 1)"
    it "evals DottedList (sugared)" $ do
      res <- Lib.test ["'((1 2) . 1)"]
      res `shouldBe` "((1 2) . 1)"
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
    it "evals string? 1" $ do
      res <- Lib.test ["(string? \"test\")"]
      res `shouldBe` "#t"
    it "evals string? 2" $ do
      res <- Lib.test ["(string? 1)"]
      res `shouldBe` "#f"
    it "evals number? 1" $ do
      res <- Lib.test ["(number? \"test\")"]
      res `shouldBe` "#f"
    it "evals number? 2" $ do
      res <- Lib.test ["(number? 1)"]
      res `shouldBe` "#t"
    it "evals symbol? 1" $ do
      res <- Lib.test ["(symbol? \"test\")"]
      res `shouldBe` "#f"
    it "evals symbol? 2" $ do
      res <- Lib.test ["(symbol? 'atom)"]
      res `shouldBe` "#t"
    it "handles conditionals" $ do
      res <- Lib.test ["(if (> 2 3) \"no\" \"yes\")"]
      res `shouldBe` "\"yes\""
