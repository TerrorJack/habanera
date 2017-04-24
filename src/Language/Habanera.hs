{-# LANGUAGE TypeFamilies #-}

module Language.Habanera where

import Data.Kind

class Lit expr where
    type LitCxt expr t :: Constraint
    type LitCxt expr t = ()
    lit
        :: LitCxt expr t
        => t -> expr t

class Eval expr where
    type EvalCxt expr t :: Constraint
    type EvalCxt expr t = ()
    type EvalResult expr t :: *
    eval
        :: EvalCxt expr t
        => expr t -> EvalResult expr t

class Func expr where
    type FuncCxt expr t0 t1 :: Constraint
    type FuncCxt expr t0 t1 = ()
    lam
        :: FuncCxt expr t0 t1
        => (expr t0 -> expr t1) -> expr (t0 -> t1)
    app
        :: FuncCxt expr t0 t1
        => expr (t0 -> t1) -> expr t0 -> expr t1

class Prod expr where
    type ProdCxt expr t0 t1 :: Constraint
    type ProdCxt expr t0 t1 = ()
    prod
        :: ProdCxt expr t0 t1
        => expr t0 -> expr t1 -> expr (t0, t1)
    deProd
        :: ProdCxt expr t0 t1
        => expr (t0, t1) -> expr (t0 -> t1 -> r) -> expr r

class CoProd expr where
    type CoProdCxt expr t0 t1 :: Constraint
    type CoProdCxt expr t0 t1 = ()
    left
        :: CoProdCxt expr t0 t1
        => expr t0 -> expr (Either t0 t1)
    right
        :: CoProdCxt expr t0 t1
        => expr t1 -> expr (Either t0 t1)
    deCoProd
        :: ProdCxt expr t0 t1
        => expr (Either t0 t1) -> expr (t0 -> r) -> expr (t1 -> r) -> expr r

class Fix expr where
    type FixCxt expr t :: Constraint
    type FixCxt expr t = ()
    fix
        :: FixCxt expr t
        => expr (t -> t) -> expr t

class Exec stmt where
    type ExecCxt stmt t :: Constraint
    type ExecCxt stmt t = ()
    type ExecResult stmt t :: *
    exec
        :: ExecCxt stmt t
        => stmt t -> ExecResult stmt t

class Ref m where
    type RefType m :: * -> *
    type RefExpr m :: * -> *
    type RefCxt m t :: Constraint
    type RefCxt m t = ()
    newRef
        :: RefCxt m t
        => RefExpr m t -> m (RefType m t)
    getRef
        :: RefCxt m t
        => RefType m t -> m (RefExpr m t)
    putRef
        :: RefCxt m t
        => RefType m t -> RefExpr m t -> m ()
