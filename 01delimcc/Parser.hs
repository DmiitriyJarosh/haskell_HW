module Parser where


import Eval  
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Foldable as Foldable

translateReset :: Term -> Term
translateReset (Reset term) = let (body, logTerm) = preEval term in
  case logTerm of
    Shift name bodyShift -> TmRst (TmAbs name body) name bodyShift
    TmCst x -> term
translateReset term = case term of
  TmVar x -> term
  TmCst x -> term
  TmAbs name body -> TmAbs name (translateReset body)
  TmAdd terml termr -> TmAdd (translateReset terml) (translateReset termr)
  TmApp terml termr -> TmApp (translateReset terml) (translateReset termr)
  TmAmb terml termr -> TmAmb (translateReset terml) (translateReset termr)
  Shift name body -> TmApp (TmCst 0) (TmCst 0) --to call mystake, no time to do it normally
  TmRst func name body -> TmRst func name (translateReset body)

addT :: Term -> Term -> Term
addT x y = case x of
  Shift name body -> x
  otherwise -> y

preEval :: Term -> (Term, Term)
preEval term = case term of
  TmVar x -> (term, TmCst 0)
  TmCst x -> (term, TmCst 0)
  TmAbs name body -> (TmAbs name (fst $ preEval body), snd $ preEval body)
  TmApp terml termr -> (TmApp (fst $ preEval terml) (fst $ preEval termr), addT (snd $ preEval terml) (snd $ preEval termr))
  TmAdd terml termr -> (TmAdd (fst $ preEval terml) (fst $ preEval termr), addT (snd $ preEval terml) (snd $ preEval termr))
  TmAmb terml termr -> (TmAmb (fst $ preEval terml) (fst $ preEval termr), addT (snd $ preEval terml) (snd $ preEval termr))
  Reset body -> preEval $ translateReset term
  Shift name body -> (TmVar name, Shift name body)
  TmRst fun name body -> (TmRst fun name (fst $ preEval body), snd $ preEval body)

languageDef = emptyDef {
                        Token.identStart = letter,
                        Token.identLetter = alphaNum,
                        Token.reservedOpNames = ["+", "-", "\\", "->", "$","#"],
                        Token.reservedNames = ["reset", "shift"]
                       }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
decimal = Token.decimal lexer
whiteSpace = Token.whiteSpace lexer

mainParser :: Parser Term
mainParser = whiteSpace >> expression



expression :: Parser Term
expression = buildExpressionParser operators term

operators = [
              [Infix (reservedOp "$" >> return TmApp) AssocLeft],
              [Infix (reservedOp "+" >> return TmAdd) AssocLeft],
              [Infix (reservedOp "#" >> return TmAmb) AssocLeft]
            ]



term :: Parser Term
term = resetParse <|> shiftParse <|> lambdaParse <|> parens expression <|> fmap TmVar identifier <|> negDecimal <|> fmap TmCst decimal

negDecimal :: Parser Term
negDecimal = do
             reservedOp "-"
             num <- decimal
             return (TmCst (negate num))


shiftParse :: Parser Term
shiftParse = do
             reserved "shift"
             whiteSpace
             nameVar <- identifier
             whiteSpace
             body <- expression
             return $ Shift nameVar body


resetParse :: Parser Term
resetParse = do
             reserved "reset"
             resetInParens <|> parens resetInParens


resetInParens :: Parser Term
resetInParens = do
                body <- expression
                return $ Reset body

lambdaParse :: Parser Term
lambdaParse = do
         reserved "\\"
         nameVar <- identifier
         reserved "->"
         body <- expression
         return $ TmAbs nameVar body

parseString :: String -> Term
parseString str = case parse mainParser "" str of
                    Left e -> error $ show e
                    Right r -> r

myParse str = translateReset $ parseString str


test1 = "1+reset(1+(shift k k$2))" -- == 1+(+1)2 == 4
test2 = "reset(1+(shift x (x$1)))" -- == 2
test3 = "(1+1)+reset(1+(shift x (x$1)))" -- == 4
test4 = "(reset(1+(shift x x)))$(1+1)" -- == 2
test5 = "(\\ x -> x + 1) $ 5" -- == 6
test6 = "(1+1)$1" --error