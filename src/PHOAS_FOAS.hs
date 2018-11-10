{-# language GADTs #-}
module PHOAS_FOAS where

{-| converting parametric higher-order abstract syntax (PHOAS) into first-order abstract syntax
https://www.reddit.com/r/haskell/comments/54pq7g/problem_about_converting_hoas_to_foas/
-}

data Exp v a where
  Var :: v a -> Exp v a
  Lam :: (v a -> Exp v b) -> Exp v (a -> b)
  App :: Exp v (a -> b) -> Exp v a -> Exp v b
  Con :: Int -> Exp v Int
  Add :: Exp v a -> Exp v a -> Exp v a


-- | dat one weird trick
newtype Const a b = Const a 



data FOAS where
  EVar   :: String -> FOAS
  ELam   :: String -> FOAS -> FOAS
  EApp   :: FOAS -> FOAS -> FOAS
  EConst :: Int -> FOAS
  EAdd :: FOAS -> FOAS -> FOAS
    deriving (Show)


-- | Doesn't typecheck if type signature is omitted 
toFOAS :: Exp (Const Int) a -> Int -> (FOAS, Int)
toFOAS eterm n =
  case eterm of
    Var (Const i) -> (EVar ("x" ++ show i), n)
    Lam f -> (ELam ("x" ++ show n) body, n + 1) where
      (body, _) = toFOAS (f (Const n)) (n + 1)
    App e u -> let
      (e', n') = toFOAS e n
      (u', u'') = toFOAS u n'
      in (EApp e' u', u'')
    Con i -> (EConst i, n)
    Add x y -> let
      (e', n') = toFOAS x n
      (u', u'') = toFOAS y n'
      in (EAdd e' u', u'')      

-- | test

example1 :: Exp v (b -> b)
example1 = Lam (\x -> Add (Var x) (Var x))

ev1 :: (FOAS, Int)
ev1 = toFOAS example1 0
