module LambdaEval
     where
type Name = String
data Term = Var String
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
        deriving (Show)

type Subst = [(Name, Term)]

subst :: Name -> Term -> Term -> Term
subst n t1 t2 = case t2 of
  Var name -> if name == n then t1 else t2
  Con num -> t2
  Add term1 term2 -> Add (subst n t1 term1) (subst n t1 term2)
  Lam name term -> if name /= n then Lam name (subst n t1 term) else t2
  App term1 term2 -> App (subst n t1 term1) (subst n t1 term2)

eval :: Term -> Term
eval t = case t of
      Var name -> t
      Lam name term1 -> t
      Con num -> t
      Add term1 term2 -> case eval term1 of
        Var name -> Add (Var name) (eval term2)
        Con num -> case eval term2 of
          Con num1 -> Con (num + num1)
          otherwise -> Add (Con num) (eval term2)
        Lam name term3 -> case subst name (eval term2) term3 of
	  Add (Con num1) (Con num2) -> Con (num1 + num2)
	  otherwise -> subst name (eval term2) term3
        App term3 term4 -> eval (App (eval term3) (eval term4))
        Add term3 term4 -> eval (Add (eval term3) (eval term4))
      App term1 term2 -> case eval term1 of
        Var name -> App (Var name) (eval term2)
        Con num -> App (Con num) (eval term2)
        Lam name term3 -> case subst name (eval term2) term3 of
	  Add (Con num1) (Con num2) -> Con (num1 + num2)
	  otherwise -> subst name (eval term2) term3
        App term3 term4 -> eval (App (eval term3) (eval term4))
        Add term3 term4 -> eval (Add (eval term3) (eval term4))
