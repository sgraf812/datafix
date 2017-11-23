{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Analyses.Templates.LetDn
  ( TransferAlgebra
  , buildProblem
  ) where

import           Data.Primitive.Array     (indexArray, sizeofArray)
import           Data.Proxy               (Proxy (..))

import           Analyses.Syntax.CoreSynF
import           Datafix

import           CoreSyn
import           VarEnv

type TransferAlgebra lattice
  = forall m
   . Monad m
  => Proxy m
  -> Proxy lattice
  -> VarEnv (TransferFunction m lattice)
  -> CoreExprF (TransferFunction m lattice)
  -> TransferFunction m lattice

buildProblem
  :: forall m
   . MonadDependency m
  => Currying (Domains (Domain m)) (CoDomain (Domain m) -> CoDomain (Domain m) -> Bool)
  => Eq (CoDomain (Domain m))
  => TransferAlgebra (Domain m)
  -> CoreExpr
  -> (Node, Node, DataFlowProblem m)
buildProblem alg e = (root, Node (sizeofArray arr - 1), DFP transfer changeDetector)
  where
    p = Proxy :: Proxy m
    changeDetector _ = eqChangeDetector p
    transfer (Node node) = indexArray arr node
    (root, arr) = runAllocator $ allocateNode $ \root_ -> do
      transferRoot <- buildRoot p alg e
      pure (root_, transferRoot)
{-# INLINE buildProblem #-}

type TF m = TransferFunction m (Domain m)

buildRoot
  :: forall m
   . MonadDependency m
  => Proxy m
  -> TransferAlgebra (Domain m)
  -> CoreExpr
  -> NodeAllocator (TF m) (TF m)
buildRoot p alg' = buildExpr emptyVarEnv
  where
    alg = alg' p (Proxy :: Proxy (Domain m))
    buildExpr
      :: VarEnv (TF m)
      -> CoreExpr
      -> NodeAllocator (TF m) (TF m)
    buildExpr env expr =
      case expr of
        Lit lit -> pure (alg env (LitF lit))
        Var id_ -> pure (alg env (VarF id_))
        Type ty -> pure (alg env (TypeF ty))
        Coercion co -> pure (alg env (CoercionF co))
        Cast e co -> do
          transferE <- buildExpr env e
          pure (alg env (CastF transferE co))
        Tick t e -> do
          transferE <- buildExpr env e
          pure (alg env (TickF t transferE))
        App f a -> do
          transferF <- buildExpr env f
          transferA <- buildExpr env a
          pure (alg env (AppF transferF transferA))
        Lam id_ body -> do
          transferBody <- buildExpr env body
          pure (alg env (LamF id_ transferBody))
        Case scrut bndr ty alts -> do
          transferScrut <- buildExpr env scrut
          transferAlts <- mapM (buildAlt env) alts
          pure (alg env (CaseF transferScrut bndr ty transferAlts))
        Let bind body -> do
          (env', transferredBind) <- registerBindingGroup env bind
          transferBody <- buildExpr env' body
          -- Note that we pass the old env to 'alg'.
          -- 'alg' should use 'transferredBind' for
          -- annotated RHSs.
          pure (alg env (LetF transferredBind transferBody))
    {-# INLINE buildExpr #-}

    buildAlt env (con, bndrs, e) = do
      transferE <- buildExpr env e
      pure (con, bndrs, transferE)
    {-# INLINE buildAlt #-}

    mapBinders f env bind = do
      let binders = flattenBinds [bind]
      (env', transferredBinds) <- f env binders
      case bind of
        Rec{} -> pure (env', RecF transferredBinds)
        NonRec{}
          | [(id_, transferRHS)] <- transferredBinds
          -> pure (env', NonRecF id_ transferRHS)
        _ -> error "NonRec, but multiple transferredBinds"
    {-# INLINE mapBinders #-}


    registerBindingGroup = mapBinders impl
      where
        impl env binders =
          case binders of
            [] -> pure (env, [])
            ((id_, rhs):binders') ->
              allocateNode $ \node -> do
                let deref = dependOn p node
                let env' = extendVarEnv env id_ deref
                (env'', transferredBind) <- impl env' binders'
                transferRHS <- buildExpr env' rhs
                -- we return `deref`, which is like `transferRHS` but
                -- with caching.
                pure ((env'', (id_, deref):transferredBind), transferRHS)
    {-# INLINE registerBindingGroup #-}
{-# INLINE buildRoot #-}
