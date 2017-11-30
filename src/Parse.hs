module Parse where

import           Control.Applicative           ((<$>))
import           Control.Monad                 (liftM)
import           Control.Monad.Except
import           Data.Maybe                    (fromJust, isJust)
import           Debug.Trace                   (trace)
import           Error
import           Primitives
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 (space <?> "spaces")

spaces' :: Parser()
spaces' = skipMany (space <?> "spaces'")

parseString :: Parser LispVal
parseString = do
               char '"'
               x <- many (noneOf "\"")
               char '"'
               return $ String x

parseAtom :: Parser LispVal
parseAtom = do
             first <- letter <|> symbol
             rest <- many (letter <|> digit <|> symbol)
             let atom = first:rest
             return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- many (do e <- parseExpr; spaces'; return e)
                y <- optionMaybe ((char '.' >> spaces >> parseExpr)  <?> "parseDotExpr failed")
                z <- if isJust y then return $ DottedSuffix $ fromJust y else return Tail
                z' <- case z of Tail           -> return $ List x
                                DottedSuffix s -> return $ DottedList x s
                char ')'
                return z'
         <?> "parseExpr!"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
   Left err  -> throwError $ Parser err
   Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of
             Bool False -> eval alt
             _          -> eval conseq
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

test :: [String] -> IO String
test args = do
   let evaled = fmap show $ readExpr (head args) >>= eval
   return $ extractValue $ trapError evaled
