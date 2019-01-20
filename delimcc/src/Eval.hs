{-# LANGUAGE LambdaCase, FlexibleInstances, InstanceSigs #-}
module Eval where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Either
import Data.List
import Data.Functor.Compose

data Term =
    TmVar String
  | TmAbs String Term
  | Nil
  | Ycomb
  | IsNil Term
  | Head Term
  | Tail Term
  | TmList Term Term
  | IfThenElse Term Term Term
  | TmAdd Term Term
  | TmApp Term Term
  | TmMult Term Term
  | TmCst Integer
  | Reset Term --only while parsing
  | Shift String Term --only while parsing
  | TmRst Term String Term
  deriving (Show, Eq)

subst :: String -> Term -> Term -> Term
subst name src dest = case dest of
  TmVar varName -> if varName == name then src else dest
  TmAbs varName term -> if varName == name then dest else TmAbs varName (subst name src term)
  TmAdd termLeft termRight -> TmAdd (subst name src termLeft) (subst name src termRight)
  TmApp termLeft termRight -> TmApp (subst name src termLeft) (subst name src termRight)
  TmMult termLeft termRight -> TmMult (subst name src termLeft) (subst name src termRight)
  TmCst num -> dest
  Nil -> dest
  Ycomb -> dest
  TmList body listTail -> TmList (subst name src body) (subst name src listTail)
  IsNil term -> IsNil (subst name src term)
  Head term -> Head (subst name src term)
  Tail term -> Tail (subst name src term)
  IfThenElse cond termT termF -> IfThenElse (subst name src cond) (subst name src termT) (subst name src termF)
  TmRst termFunc varName termBody -> if varName == name
    then TmRst (subst name src termFunc) varName termBody
    else TmRst (subst name src termFunc) varName (subst name src termBody)

type Error = String

combY = TmAbs "f" (TmApp (TmAbs "x" (TmApp (TmVar "f") (TmApp (TmVar "x") (TmVar "x")))) (TmAbs "x" (TmApp (TmVar "f") (TmApp (TmVar "x") (TmVar "x")))))

eval :: Term -> Either Error Term
eval expr = case expr of
    TmVar name -> return expr
    Ycomb -> return expr
    Nil -> return expr
    TmAbs name body -> return expr
    TmCst num -> return expr
    TmAdd terml termr -> eval terml >>= (\case
        Nil -> fail ("Unexpected 'Nil'")
        Ycomb -> fail "Unexpected Ycomb combinator"
        TmList body listTail -> fail ("Unexpected list")
        TmVar name -> fail ("Unbound variable " ++ name)
        TmCst numl -> eval termr >>= (\case
            TmCst numr -> return (TmCst (numl + numr))
            Nil -> fail ("Unexpected 'Nil'")
            Ycomb -> fail "Unexpected Ycomb combinator"
            TmList body listTail -> fail ("Unexpected list")
            TmVar name -> fail ("Unbound variable " ++ name)
            term -> fail ("Should be numbers: " ++ show numl ++ " and " ++ show term))
        TmAbs name_lam term_lam -> fail ("Should be numbers: " ++ show (TmAbs name_lam term_lam) ++ " and " ++ show termr))
    TmMult terml termr -> eval terml >>= (\case
      Nil -> fail ("Unexpected 'Nil'")
      Ycomb -> fail "Unexpected Ycomb combinator"
      TmList body listTail -> fail ("Unexpected list")
      TmVar name -> fail ("Unbound variable " ++ name)
      TmCst numl -> eval termr >>= (\case
          TmCst numr -> return (TmCst (numl * numr))
          Ycomb -> fail "Unexpected Ycomb combinator"
          Nil -> fail ("Unexpected 'Nil'")
          TmList body listTail -> fail ("Unexpected list")
          TmVar name -> fail ("Unbound variable " ++ name)
          term -> fail ("Should be numbers: " ++ show numl ++ " and " ++ show term))
      TmAbs name_lam term_lam -> fail ("Should be numbers: " ++ show (TmAbs name_lam term_lam) ++ " and " ++ show termr))
    TmApp terml termr -> eval terml >>= (\case
        TmVar name -> fail ("`" ++ name ++ "` is not a function")
        TmList body listTail -> fail ("Unexpected list")
        Nil -> fail ("Unexpected 'Nil'")
        TmCst num -> fail ("`" ++ show num ++ "` is not a function")
        Ycomb -> eval (TmApp combY termr)
        TmAbs name term3 -> eval (subst name termr term3))
    IfThenElse cond termT termF -> eval cond >>= (\case
      TmCst x -> if x /= 0 then eval termT else eval termF
      _ -> fail (show cond ++" cannot be condition in if statement"))
    TmRst termFunc varName termBody -> eval (subst varName termFunc termBody)
    TmList body listTail -> eval body >>= (\x -> eval listTail >>= return . TmList x)
    Head term -> eval term >>= \case
      TmList body listTail -> return body
      Nil -> return Nil
      _ -> fail "The argument of `Head` is not a list"
    Tail term -> eval term >>= \case
      TmList body listTail -> return listTail
      Nil -> return Nil
      _ -> fail "The argument of `Tail` is not a list"
    IsNil term -> eval term >>= \case
      Nil -> return (TmCst 1)
      TmList body listTail -> return (TmCst 0)
      _ -> fail "The argument of `IsNil` is not a list"


test1 = TmRst (TmAbs "t" (TmAdd (TmCst 1) (TmVar "t"))) "x" (TmApp (TmVar "x") (TmCst 1)) -- reset (1 + (shift x (x 1))) == Right [TmCst () 2] == 2
test2 = TmAdd (TmRst (TmAbs "t" (TmAdd (TmCst 1) (TmVar "t"))) "x" (TmApp (TmVar "x") (TmCst 1))) (TmAdd (TmCst 1) (TmCst 1)) -- == 4
test3 = TmApp (TmRst (TmAbs "t" (TmAdd (TmCst 1) (TmVar "t"))) "x" (TmVar "x")) (TmAdd (TmCst 1) (TmCst 1)) -- == 3
test4 = TmAdd (TmRst (TmAdd (TmCst 1) (TmCst 1)) "x" (TmApp (TmVar "x") (TmCst 1))) (TmAdd (TmCst 1) (TmCst 1)) -- == Error
