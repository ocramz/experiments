{-# language GADTs #-}

module CompilingEmbeddedLanguages where


data Expr a where
  Lift :: a -> Expr a
  Lam :: (Expr a -> Expr b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b
  -- Tup :: Expr a -> Expr b -> Expr (a, b)
  -- ProjL :: Expr (a, b) -> Expr a
  -- ProjR :: Expr (a, b) -> Expr b



-- lift2 :: (Expr a -> Expr b -> Expr c) -> Expr (a -> b -> c)
  
eval :: Expr a -> a
eval (Lift x) = x
eval (Lam f) = \x -> eval (f (Lift x))
eval (App f x) = (eval f) (eval x)
-- eval (Tup l r) = (eval l, eval r)
-- eval (ProjL (l, r)) = l

lift :: a -> Expr a
lift = Lift

lam :: (Expr a -> Expr b) -> Expr (a -> b)
lam = Lam

(.$.) :: Expr (a -> b) -> Expr a -> Expr b
f .$. x = App f x


    


-- pretty (Lift x) = show x
-- pretty (Add e1 e2) = unwords [pretty e1, "+", pretty e2]



-- ex0 = Add (Lift 1) (Lift 2)
