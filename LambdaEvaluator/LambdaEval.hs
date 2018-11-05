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
  Add term1 term2 -> (Add (fst $ substT1) (fst $ substT2), max (snd $ substT1) (snd $ substT2))
    where
      substT1 = subst n t1 term1
      substT2 = subst n t1 term2
  Lam name term -> if name /= n then (Lam name (fst $ substT), snd $ substT) else (t2, 0)
    where
      substT = subst n t1 term
  App term1 term2 -> (App (fst $ substT1) (fst $ substT2), max (snd $ substT1) (snd $ substT2))
    where
      substT1 = subst n t1 term1
      substT2 = subst n t1 term2

eval :: Term -> Int -> (Term, Int)
eval t cnt = case t of
      Var name -> (t, cnt)
      Lam name term1 -> (t, cnt)
      Con num -> (t, cnt)
      Add term1 term2 -> case fst $ eval term1 0 of
        Var name -> (Add (Var name) (fst $ evalTerm2), (snd $ evalTerm2) + cnt)
        Con num -> case fst $ evalTerm2 of
          Con num1 -> (Con (num + num1), (snd $ evalTerm2) + cnt + 1)
          otherwise -> (Add (Con num) (fst $ evalTerm2), (snd $ evalTerm2) + cnt)
        Lam name term3 -> (t, cnt)
        App term3 term4 -> (fst $ eval appTerm 0, snd $ eval appTerm reduxSum)
          where
            evalTerm3 = eval term3 0
            evalTerm4 = eval term4 0
            appTerm = App (fst $ evalTerm3) (fst $ evalTerm4)
            addTerm = Add (fst $ evalTerm3) (fst $ evalTerm4)
            reduxSum = (snd $ evalTerm3) + (snd $ evalTerm4)
        Add term3 term4 -> (fst $ eval addTerm 0, snd $ eval addTerm reduxSum)
          where
            evalTerm3 = eval term3 0
            evalTerm4 = eval term4 0
            appTerm = App (fst $ evalTerm3) (fst $ evalTerm4)
            addTerm = Add (fst $ evalTerm3) (fst $ evalTerm4)
            reduxSum = (snd $ evalTerm3) + (snd $ evalTerm4)
        where
          evalTerm2 = eval term2 0


      App term1 term2 -> case fst $ eval term1 0 of
        Var name -> (App (Var name) (fst $ evalTerm2), cnt + (snd $ evalTerm2))
        Con num -> (App (Con num) (fst $ evalTerm2), cnt + (snd $ evalTerm2))
        Lam name term3 -> (fst $ eval (fst $ substT2toT3) 0, cnt + (snd $ eval (fst $ substT2toT3) ((snd $ evalTerm2) + (snd $ substT2toT3))))
          where
            substT2toT3 = subst name (fst $ evalTerm2) term3
        App term3 term4 -> (fst $ eval appTerm 0, snd $ eval appTerm reduxSum)
          where
            evalTerm3 = eval term3 0
            evalTerm4 = eval term4 0
            appTerm = App (fst $ evalTerm3) (fst $ evalTerm4)
            addTerm = Add (fst $ evalTerm3) (fst $ evalTerm4)
            reduxSum = (snd $ evalTerm3) + (snd $ evalTerm4)
        Add term3 term4 -> (fst $ eval addTerm 0, snd $ eval addTerm reduxSum)
          where
            evalTerm3 = eval term3 0
            evalTerm4 = eval term4 0
            appTerm = App (fst $ evalTerm3) (fst $ evalTerm4)
            addTerm = Add (fst $ evalTerm3) (fst $ evalTerm4)
            reduxSum = (snd $ evalTerm3) + (snd $ evalTerm4)
        where
          evalTerm2 = eval term2 0
