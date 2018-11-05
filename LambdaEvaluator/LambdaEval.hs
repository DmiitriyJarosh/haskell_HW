module LambdaEval
     where
type Name = String
data Term = Var String
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
        deriving (Show)

subst :: Name -> Term -> Term -> (Term, Int)
subst n t1 t2 = case t2 of
  Var name -> if name == n then (t1, 1) else (t2, 0)
  Con num -> (t2, 0)
  Add term1 term2 -> (Add (fst $ subst n t1 term1) (fst $ subst n t1 term2), max (snd $ subst n t1 term1) (snd $ subst n t1 term2))
  Lam name term -> if name /= n then (Lam name (fst $ subst n t1 term), snd $ subst n t1 term) else (t2, 0)
  App term1 term2 -> (App (fst $ subst n t1 term1) (fst $ subst n t1 term2), max (snd $ subst n t1 term1) (snd $ subst n t1 term2))


eval :: Term -> Int -> (Term, Int)
eval t cnt = case t of
      Var name -> (t, cnt)
      Lam name term1 -> (t, cnt)
      Con num -> (t, cnt)
      Add term1 term2 -> case fst $ eval term1 0 of
        Var name -> (Add (Var name) (fst $ eval term2 0), (snd $ eval term2 0) + cnt)
        Con num -> case fst $ eval term2 0 of
          Con num1 -> (Con (num + num1), (snd $ eval term2 0) + cnt + 1)
          otherwise -> (Add (Con num) (fst $ eval term2 0), (snd $ eval term2 0) + cnt)
        Lam name term3 -> (t, cnt)
        App term3 term4 -> (fst $ eval (App (fst $ eval term3 0) (fst $ eval term4 0)) 0, snd $ eval (App (fst $ eval term3 0) (fst $ eval term4 0)) ((snd $ eval term3 0) + (snd $ eval term4 0)))
        Add term3 term4 -> (fst $ eval (Add (fst $ eval term3 0) (fst $ eval term4 0)) 0, snd $ eval (Add (fst $ eval term3 0) (fst $ eval term4 0)) ((snd $ eval term3 0) + (snd $ eval term4 0)))
      App term1 term2 -> case fst $ eval term1 0 of
        Var name -> (App (Var name) (fst $ eval term2 0), cnt + (snd $ eval term2 0))
        Con num -> (App (Con num) (fst $ eval term2 0), cnt + (snd $ eval term2 0))
        Lam name term3 -> (fst $ eval (fst $ subst name (fst $ eval term2 0) term3) 0, cnt + (snd $ eval (fst $ subst name (fst $ eval term2 0) term3) ((snd $ eval term2 0) + (snd $ subst name (fst $ eval term2 0) term3))))
        App term3 term4 -> (fst $ eval (App (fst $ eval term3 0) (fst $ eval term4 0)) 0, snd $ eval (App (fst $ eval term3 0) (fst $ eval term4 0)) ((snd $ eval term3 0) + (snd $ eval term4 0)))
        Add term3 term4 -> (fst $ eval (Add (fst $ eval term3 0) (fst $ eval term4 0)) 0, snd $ eval (Add (fst $ eval term3 0) (fst $ eval term4 0)) ((snd $ eval term3 0) + (snd $ eval term4 0)))
