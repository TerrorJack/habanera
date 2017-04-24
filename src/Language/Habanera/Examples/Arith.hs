{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Habanera.Examples.Arith where

import qualified Language.Habanera as H

data BinOp
    = Add
    | Sub
    | Mul
    | Div

data Frame
    = Lit Double
    | BinAp BinOp

newtype GenStack t = GenStack
    { runGenStack :: [Frame] -> [Frame]
    }

instance H.Lit GenStack where
    type LitCxt GenStack t = (t ~ Double)
    lit x = GenStack (\s -> Lit x : s)

binAp :: BinOp -> GenStack Double -> GenStack Double -> GenStack Double
binAp op (GenStack f0) (GenStack f1) = GenStack (\s -> BinAp op : f0 (f1 s))

instance Num (GenStack Double) where
    (+) = binAp Add
    (-) = binAp Sub
    (*) = binAp Mul
    fromInteger = H.lit . fromInteger

compile :: GenStack Double -> [Frame]
compile (GenStack f) = f []

exec :: [Frame] -> Double
exec s =
    let (v, r) = w s
    in case r of
           [] -> v
           _ -> error "residues exist"
  where
    w :: [Frame] -> (Double, [Frame])
    w (Lit v:r) = (v, r)
    w (BinAp op:r) =
        let (x, rx) = w r
        in let (y, ry) = w rx
           in (f op x y, ry)
    w [] = error "empty stack"
    f :: BinOp -> Double -> Double -> Double
    f Add = (+)
    f Sub = (-)
    f Mul = (*)
    f Div = (/)

test_case :: GenStack Double
test_case = 2 + 3 * 5

test_result :: Double
test_result = exec (compile test_case)
