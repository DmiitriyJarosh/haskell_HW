module LambdaEval
     where
type Name = String
data Term = Var String
          | Lam Name Term
          | App Term Term
        deriving (Show)

type Subst = [(Name, Term)]

subst :: Name -> Term -> Term -> Term
subst n t1 t2 = case t2 of
  Var name -> if name == n then t1 else t2
  Lam name term -> if name /= n then Lam name (subst n t1 term) else t2
  App term1 term2 -> App (subst n t1 term1) (subst n t1 term2)

eval :: Term -> Term
eval t = case t of
      Var name -> t
      Lam name term1 -> t
      App term1 term2 -> case eval term1 of
        Var name -> App (Var name) term2
        Lam name term3 -> subst name term2 term3
        App term3 term4 -> eval (App (eval term3) term4)
