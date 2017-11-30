import           Control.Exception (evaluate)
import           Parse             (test)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Evaluation 1" $ do
    it "evals Atom" $ do
      res <- test ["'atom"]
      res `shouldBe` "atom"
    it "evals quoted Atom" $ do
      res <- test ["'(atom)"]
      res `shouldBe` "(atom)"
    it "evals String" $ do
      res <- test ["\"a string\""]
      res `shouldBe` "\"a string\""
    it "evals Bool (atom)" $ do
      res <- test ["#t"]
      res `shouldBe` "#t"
    it "evals DottedList" $ do
      res <- test ["'(1 2 . 1)"]
      res `shouldBe` "(1 2 . 1)"
    it "evals DottedList (nested)" $ do
      res <- test ["'((1 1 . 2) (2 3) . 1)"]
      res `shouldBe` "((1 1 . 2) (2 3) . 1)"
    it "evals simple numericBinop" $ do
      res <- test["(+ 2 2)"]
      res  `shouldBe` "4"
    it "evals compound numericBinop" $ do
      res <- test["(+ 2 (+ 3 2))"]
      res  `shouldBe` "7"
  describe "Error Checking" $ do
    it "throws Invalid type error 1" $ do
      res <- test ["(+ 2 \"two\")"]
      res `shouldBe` "Invalid type: expected number, found \"two\""
    it "throws arg error" $ do
      res <- test ["(+ 2)"]
      res `shouldBe` "Expected 2 args; found values 2"
    it "throws unrecognized fn error" $ do
      res <- test["(what? 2)"]
      res  `shouldBe` "Unrecognized primitive function args: \"what?\""
  describe "BinOps" $ do
    it "evals boolBinop" $ do
      res <- test ["(< 2 3)"]
      res `shouldBe` "#t"
    it "evals boolBinop 2" $ do
      res <- test ["(< 3 2)"]
      res `shouldBe` "#f"
    it "evals boolBinop 3" $ do
      res <- test ["(> 3 2)"]
      res `shouldBe` "#t"
    it "evals boolBinop 4" $ do
      res <- test ["(> 2 3)"]
      res `shouldBe` "#f"
    it "evals strBoolBinop" $ do
      res <- test ["(string=? \"test\"  \"test\")"]
      res `shouldBe` "#t"
  describe "Evaluation 2" $ do
    it "evals string? 1" $ do
      res <- test ["(string? \"test\")"]
      res `shouldBe` "#t"
    it "evals string? 2" $ do
      res <- test ["(string? 1)"]
      res `shouldBe` "#f"
    it "evals number? 1" $ do
      res <- test ["(number? \"test\")"]
      res `shouldBe` "#f"
    it "evals number? 2" $ do
      res <- test ["(number? 1)"]
      res `shouldBe` "#t"
    it "evals symbol? 1" $ do
      res <- test ["(symbol? \"test\")"]
      res `shouldBe` "#f"
    it "evals symbol? 2" $ do
      res <- test ["(symbol? 'atom)"]
      res `shouldBe` "#t"
    it "handles conditionals" $ do
      res <- test ["(if (> 2 3) \"no\" \"yes\")"]
      res `shouldBe` "\"yes\""
  describe "List Operations" $ do
    it "cdr works" $ do
      res <- test ["(cdr '(a simple test))"]
      res `shouldBe` "(simple test)"
    it "car works" $ do
      res <- test ["(car (cdr '(a simple test)))"]
      res `shouldBe` "simple"
    it "car works nested" $ do
      res <- test ["(car '((this is) a test)))"]
      res `shouldBe` "(this is)"
    it "cons works" $ do
      res <- test ["(cons '(this is) 'test)"]
      res `shouldBe` "((this is) . test)"
    it "cons works with empty list" $ do
      res <- test ["(cons '(this is) '())"]
      res `shouldBe` "((this is))"
  describe "Equivalence Operations" $ do
    it "eqv works 1" $ do
      res <- test ["(eqv? 1 3)"]
      res `shouldBe` "#f"
    it "eqv works 2" $ do
      res <- test ["(eqv? 3 3)"]
      res `shouldBe` "#t"
    it "eqv works 3" $ do
      res <- test ["(eqv? 'atom 'atom)"]
      res `shouldBe` "#t"
