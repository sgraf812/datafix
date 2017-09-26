{-# LANGUAGE ScopedTypeVariables #-}
-- This is so that the specialisation of transferFunctionAlg gets inlined.
{-# OPTIONS_GHC -funfolding-creation-threshold=999999 #-}

module Analyses.StrAnal.Analysis where

import           Algebra.Lattice
import           Analyses.StrAnal.Arity
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.CoreSynF
import           Analyses.Templates.LetDn
import           Control.Monad               (foldM)
import           Datafix.Worklist            (Density (..), IterationBound (..),
                                              fixProblem)

import           CoreSyn
import           Id
import           Var
import           VarEnv

analyse :: CoreExpr -> StrLattice
analyse expr = fixProblem problem Sparse NeverAbort root 0
  where
    (root, problem) = buildProblem transferFunctionAlg expr

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f  = f
applyWhen False _ = id

transferFunctionAlg :: TransferAlgebra (Arity -> StrLattice)
transferFunctionAlg _ _ env expr arity =
  case expr of
    LitF _       -> pure emptyStrLattice
    TypeF _      -> pure emptyStrLattice
    -- Coercions are irrelevant to Strictness Analysis:
    -- 'emptyStrLattice' is already the 'top' element,
    -- so it's a safe approximation.
    CoercionF _ -> pure emptyStrLattice
    TickF _ e    -> e arity
    CastF e _ -> e arity
    AppF f a -> do
      StrLattice (fTy, fAnns) <- f (arity + 1)
      let (argStr, fTy') = overArgs unconsArgStr fTy
      let argArity =
            case argStr of
              -- It's unfortunate that we don't have the type available to
              -- trim this... But it doesn't hurt either.
              HyperStrict -> Arity maxBound
              Lazy        -> 0
              Strict n    -> n
      StrLattice (aTy, aAnns) <- a argArity
      pure (mkStrLattice (aTy `bothStrType` fTy') (fAnns \/ aAnns))
    VarF id_
      | isLocalId id_ -> do
          rhsType <- case lookupVarEnv env id_ of
            Just denotation -> strType <$> denotation arity
            Nothing         -> pure emptyStrType
          pure (mkStrLattice (unitStrType id_ (Strict arity) `bothStrType` rhsType) emptyAnnotations)
      | otherwise -> pure emptyStrLattice
    LamF id_ body
      | isTyVar id_ -> body arity
      | otherwise -> do
          StrLattice (ty1, anns) <- body (0 /\ (arity-1))
          let (argStr, ty2) = peelFV id_ ty1
          let anns' = annotate id_ argStr anns
          let ty3 = modifyArgs (consArgStr argStr) ty2
          let ty4 = applyWhen (arity == 0) lazifyStrType ty3
          pure (mkStrLattice ty4 anns')
    CaseF scrut bndr _ alts -> do
      let transferAlt (_, bndrs, transfer) = do
            latt <- transfer arity
            pure (peelAndAnnotateFVs bndrs latt)
      StrLattice (altTy, altAnns) <-
        peelAndAnnotateFV bndr . joins <$> mapM transferAlt alts
      StrLattice (scrutTy, scrutAnns) <- scrut 0
      pure (mkStrLattice (scrutTy `bothStrType` altTy) (scrutAnns \/ altAnns))
    LetF bind body -> do
      let transferBinder (StrLattice (ty, anns)) (id_, transfer) = do
            -- We do this only for annotations.
            -- Strictness on free variables was unleashed
            -- at call sites, now we only have to
            -- 'transfer' with the minimum incoming arity.
            -- Well, actually the minimum possible arity
            -- for which we annotate is 'idArity'.
            -- This is OK as long as the function is only
            -- called through the wrapper and as long as
            -- this wrapper is only inlined when fully
            -- saturated.
            -- Otherwise, to account for unsaturated calls,
            -- we'd always have to assume incoming arity 0
            -- for annotations, which wouldn't allow us to
            -- unbox any arguments.
            let (str, ty') = peelFV id_ ty
            let anns' = annotate id_ str anns
            let oldArity = Arity (idArity id_)
            let safeArity
                  | Strict n <- str = n
                  | otherwise = 0
            let annotationArity = oldArity /\ safeArity
            StrLattice (_, rhsAnns) <- transfer annotationArity
            pure (mkStrLattice ty' (anns' \/ rhsAnns))
      latt <- body arity
      foldM transferBinder latt (flattenBindsF [bind])
