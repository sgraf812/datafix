{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Analyses.Templates.LetDn where

import qualified Data.IntMap.Lazy         as IntMap
import           Data.Maybe               (fromMaybe)
import           Data.Proxy               (Proxy (..))

import           Analyses.Syntax.CoreSynF
import           Datafix
import           Datafix.NodeAllocator
import           Datafix.Utils.TypeLevel

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
  :: forall lattice
   . Eq (CoDomain lattice)
  => Datafixable lattice
  => TransferAlgebra lattice
  -> CoreExpr
  -> (GraphNode, DataFlowProblem lattice)
buildProblem alg e = (root, DFP transfer changeDetector)
  where
    p = Proxy :: Proxy lattice
    changeDetector _ = eqChangeDetector p
    notFoundError = error "Requested a node that wasn't present in the graph"
    transfer (GraphNode node) = fromMaybe notFoundError (IntMap.lookup node map_)
    (root, map_) = runAllocator $ allocateNode $ \root_ -> do
      transferRoot <- buildRoot p alg e
      pure (root_, transferRoot)

type TF lattice = TransferFunction (DependencyM lattice) lattice

buildRoot
  :: forall lattice
   . Datafixable lattice
  => Proxy lattice
  -> TransferAlgebra lattice
  -> CoreExpr
  -> NodeAllocator (TF lattice) (TF lattice)
buildRoot p alg' = buildExpr emptyVarEnv
  where
    alg = alg' (Proxy :: Proxy (DependencyM lattice)) p
    buildExpr
      :: VarEnv (TF lattice)
      -> CoreExpr
      -> NodeAllocator (TF lattice) (TF lattice)
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

    buildAlt env (con, bndrs, e) = do
      transferE <- buildExpr env e
      pure (con, bndrs, transferE)

    mapBinders f env bind = do
      let binders = flattenBinds [bind]
      (env', transferredBinds) <- f env binders
      case bind of
        Rec{} -> pure (env', RecF transferredBinds)
        NonRec{}
          | [(id_, transferRHS)] <- transferredBinds
          -> pure (env', NonRecF id_ transferRHS)
        _ -> error "NonRec, but multiple transferredBinds"

    registerBindingGroup = mapBinders impl
      where
        impl env binders =
          case binders of
            [] -> pure (env, [])
            ((id_, rhs):binders') ->
              allocateNode $ \node -> do
                let deref = dependOn (Proxy :: Proxy lattice) node
                let env' = extendVarEnv env id_ deref
                (env'', transferredBind) <- impl env' binders'
                transferRHS <- buildExpr env' rhs
                -- we return `deref`, which is like `transferRHS` but
                -- with caching.
                pure ((env'', (id_, deref):transferredBind), transferRHS)
