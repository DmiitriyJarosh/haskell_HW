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
  | TmAdd Term Term
  | TmApp Term Term
  | TmCst Integer
  | Reset Term
  | Shift String Term
  | TmAmb Term Term
  | TmRst Term String Term
  deriving Show

subst :: String -> Term -> Term -> Term
subst name src dest = case dest of
  TmVar varName -> if varName == name then src else dest
  TmAbs varName term -> if varName == name then dest else TmAbs varName (subst name src term)
  TmAdd termLeft termRight -> TmAdd (subst name src termLeft) (subst name src termRight)
  TmApp termLeft termRight -> TmApp (subst name src termLeft) (subst name src termRight)
  TmCst num -> dest
  TmAmb termLeft termRight -> TmAmb (subst name src termLeft) (subst name src termRight)
  TmRst termFunc varName termBody -> if varName == name
    then TmRst (subst name src termFunc) varName termBody
    else TmRst (subst name src termFunc) varName (subst name src termBody)


instance Monad (Compose (Either String) []) where
  return :: a -> Compose (Either String) [] a
  return x = Compose $ Right [x]
  fail :: String -> Compose (Either String) [] a
  fail msg = Compose $ Left msg
  (>>=) :: Compose (Either String) [] a -> (a -> Compose (Either String) [] b) -> Compose (Either String) [] b
  x >>= f = Compose $ case getCompose x of
                          Left msg -> Left msg
                          Right xs -> let res = (map (getCompose . f) xs) in
                                        if any isLeft res then head $ filter isLeft res
                                          else Right $ concatMap (fromRight . getCompose . f) xs
                                          where
                                            -- use only with Right!!!
                                            fromRight :: Either String [a]-> [a]
                                            fromRight (Right x) = x

instance MonadPlus (Compose (Either String) []) where
  mzero :: Compose (Either String) [] a
  mzero = Compose $ Right []
  mplus :: Compose (Either String) [] a -> Compose (Either String) [] a -> Compose (Either String) [] a
  Compose (Right x) `mplus` Compose (Right y) = Compose $ Right (x ++ y)
  Compose (Left err1) `mplus` Compose (Right y) = Compose (Left err1)
  Compose (Right x) `mplus` Compose (Left err2) = Compose (Left err2)
  Compose (Left err1) `mplus` Compose (Left err2) = Compose $ Left (err1 ++ "; " ++ err2)





eval :: Term -> (Compose (Either String) []) (Term)
eval expr = case expr of
    TmVar name -> return expr
    TmAbs name body -> return expr
    TmCst num -> return expr
    TmAdd terml termr -> eval terml >>= (\case
        TmVar name -> fail ("Unbound variable " ++ name)
        TmCst numl -> eval termr >>= (\case
            TmCst numr -> return (TmCst (numl + numr))
            TmAmb term1 term2 -> return term1 `mplus` return term2 >>= (eval . TmAdd (TmCst numl))
            TmRst termFunc varName termBody -> eval (TmAdd (TmCst numl) (subst varName termFunc termBody))
            term -> fail ("Should be numbers: " ++ show numl ++ " and " ++ show term))
        TmAbs name_lam term_lam -> fail ("Should be numbers: " ++ show (TmAbs name_lam term_lam) ++ " and " ++ show termr)
        TmApp term3 term4 -> eval (TmApp term3 term4)
        TmAdd term3 term4 -> eval (TmAdd term3 term4)
        TmAmb term1 term2 -> return term1 `mplus` return term2 >>= (\k -> eval (TmAdd k termr))
        TmRst termFunc varName termBody -> eval (TmAdd (subst varName termFunc termBody) termr))
    TmApp terml termr -> eval terml >>= (\case
        TmVar name -> fail ("`" ++ name ++ "` is not a function")
        TmCst num -> fail ("`" ++ show num ++ "` is not a function")
        TmAbs name term3 -> eval (subst name termr term3)
        TmApp term3 term4 -> eval (TmApp term3 term4)
        TmAdd term3 term4 -> eval (TmAdd term3 term4)
        TmAmb term1 term2 -> return term1 `mplus` return term2 >>= (\x -> eval (TmApp x termr))
        TmRst termFunc varName termBody -> eval (TmApp (subst varName termFunc termBody) termr))
    TmAmb term1 term2 -> return term1 `mplus` return term2 >>= eval
    TmRst termFunc varName termBody -> eval (subst varName termFunc termBody)

	
mainEval term = getCompose $ eval term

test1 = TmRst (TmAbs "t" (TmAdd (TmCst 1) (TmVar "t"))) "x" (TmApp (TmVar "x") (TmCst 1)) -- reset (1 + (shift x (x 1))) == Right [TmCst () 2] == 2
test2 = TmAdd (TmRst (TmAbs "t" (TmAdd (TmCst 1) (TmVar "t"))) "x" (TmApp (TmVar "x") (TmCst 1))) (TmAdd (TmCst 1) (TmCst 1)) -- == 4
test3 = TmApp (TmRst (TmAbs "t" (TmAdd (TmCst 1) (TmVar "t"))) "x" (TmVar "x")) (TmAdd (TmCst 1) (TmCst 1)) -- == 3
test4 = TmAdd (TmRst (TmAdd (TmCst 1) (TmCst 1)) "x" (TmApp (TmVar "x") (TmCst 1))) (TmAdd (TmCst 1) (TmCst 1)) -- == Error
