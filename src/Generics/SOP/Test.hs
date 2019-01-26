{-# language DeriveGeneric #-}
{-# language DataKinds, GADTs, RankNTypes, KindSignatures #-}
{-# language FlexibleContexts #-}
module Generics.SOP.Test where

import Data.Function (on)
import qualified GHC.Generics as G
import Generics.SOP 

data T1 = A | B | C deriving (G.Generic, Eq, Show)
instance Generic T1

data T2 = T2 { t21 :: T1 , t22 :: Either Int Char } deriving (G.Generic, Eq, Show)
instance Generic T2



testT2 :: T2
testT2 = T2 B (Left 42)

-- | encoded "
-- testT2enc :: SOP I '['[T1, Either Int Char]]
testT2enc = from testT2


-- baz xs ys = hcliftA2 p eq xs ys where
--   p :: Proxy Eq
--   p = Proxy
--   eq :: forall (a :: *). Eq a => I a -> I a -> K Bool a
--   eq (I a) (I b) = K (a == b)


geq :: (Generic a, All2 Eq (Code a)) => a -> a -> Bool
geq = go `on` from
  where
    go :: forall xss. (All2 Eq xss, All SListI xss) => SOP I xss -> SOP I xss -> Bool
    go (SOP (Z xs))  (SOP (Z ys))  = and . hcollapse $ hcliftA2 p eq xs ys
    go (SOP (S xss)) (SOP (S yss)) = go (SOP xss) (SOP yss)
    go _             _             = False

    p :: Proxy Eq
    p = Proxy

    eq :: forall (a :: *) . Eq a => I a -> I a -> K Bool a
    eq (I a) (I b) = K (a == b)  
