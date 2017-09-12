{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analyses.Templates.LetDn where

import           Algebra.Lattice
import           Data.Maybe                       (fromMaybe)

import           Analyses.Templates.NodeAllocator
import           Datafix

import           Analyses.Syntax.CoreSynF
import           BasicTypes
import           CoreSyn
import qualified Data.IntMap.Lazy                 as IntMap
import           VarEnv

buildProblem
  :: (Eq v, BoundedJoinSemiLattice v)
  => (forall m. Monad m => VarEnv (Arity -> m v) -> CoreExprF (Arity -> m v) -> Arity -> m v)
  -> CoreExpr
  -> (ExprNode, DataFlowProblem (ExprNode, Arity) v)
buildProblem alg e = (root, DFP transfer (const eqChangeDetector))
  where
    notFoundError = error "Requested a node that wasn't present in the graph"
    transfer (ExprNode node, arity) =
      ($ arity)
      . fromMaybe notFoundError
      . IntMap.lookup node
      $ map_
    (root, map_) = runAllocator $ allocateNode $ \root_ -> do
      transferRoot <- buildRoot alg e
      pure (root_, transferRoot)

type TF v a = TransferFunction (ExprNode, Arity) v a

buildRoot
  :: forall v. BoundedJoinSemiLattice v
  => (forall m. Monad m => VarEnv (Arity -> m v) -> CoreExprF (Arity -> m v) -> Arity -> m v)
  -> CoreExpr
  -> NodeAllocator (Arity -> TF v v) (Arity -> TF v v)
buildRoot alg = buildExpr emptyVarEnv
  where
    buildExpr
      :: VarEnv (Arity -> TF v v)
      -> CoreExpr
      -> NodeAllocator (Arity -> TF v v) (Arity -> TF v v)
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
                let deref arity = dependOn (node, arity)
                let env' = extendVarEnv env id_ deref
                (env'', transferredBind) <- impl env' binders'
                transferRHS <- buildExpr env' rhs
                -- we return `deref`, which is like `transferRHS` but
                -- with caching.
                pure ((env'', (id_, deref):transferredBind), transferRHS)
