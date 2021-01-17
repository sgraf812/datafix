{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- This is so that the specialisation of transferFunctionAlg gets inlined.
{-# OPTIONS_GHC -funfolding-creation-threshold=999999 #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-idinfo #-}

-- | This module defines a strictness analysis in the style of GHC's
-- projection-based backwards analysis by defining a 'transferFunctionAlg'
-- that is passed on to @Analyses.Templates.LetDn.'buildFramework'@,
-- yielding a 'DataFlowFramework' to be solved by @Datafix.'solveProblem'@.
module Analyses.StrAnal.Analysis (analyse) where

import           Datafix.Utils.SemiLattice
import           Analyses.StrAnal.Arity
import           Analyses.StrAnal.Strictness
import           Analyses.Syntax.CoreSynF
import           Analyses.Templates.LetDn
import           Control.Monad               (foldM)
import           Datafix.Worklist            (IterationBound (..),
                                              evalDenotation)

import           CoreSyn
import           Id
import           Var
import           VarEnv

analyse :: CoreExpr -> StrLattice
analyse expr = evalDenotation @(Arity -> StrLattice) (buildDenotation transferFunctionAlg expr) NeverAbort (0 :: Arity)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f  = f
applyWhen False _ = id

-- | This specifies the strictness as a 'TransferAlgebra'. Note the absence
-- of any recursion! That's all abstracted into
-- @Analyses.Tempaltes.LetDn.'buildFramework'@, so that this function definition
-- is completely compositional: It is only concerned with peeling off a single
-- layer of the 'CoreExprF' and interpret that in terms of the
-- transfer function over the @Arity -> StrLattice@ 'Domain'.
--
-- Because there is no explicit fixpointing going on, the resulting analysis
-- logic is clear and to the point.
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
      StrLattice fTy fAnns <- f (arity + 1)
      let (argStr, fTy') = overArgs unconsArgStr fTy
      let argArity =
            case argStr of
              -- It's unfortunate that we don't have the type available to
              -- trim this... But it doesn't hurt either.
              HyperStrict -> Arity maxBound
              Lazy        -> 0
              Strict n    -> n
      StrLattice aTy aAnns <- a argArity
      pure (mkStrLattice (aTy `bothStrType` fTy') (fAnns \/ aAnns))
    VarF id_
      | isLocalId id_ -> do
          rhsType <- case lookupVarEnv env id_ of
            Just denotation -> strTy <$> denotation arity
            Nothing         -> pure emptyStrType
          pure (mkStrLattice (unitStrType id_ (Strict arity) `bothStrType` rhsType) emptyAnnotations)
      | otherwise -> pure emptyStrLattice
    LamF id_ body
      | isTyVar id_ -> body arity
      | otherwise -> do
          StrLattice ty1 anns <- body (0 /\ (arity-1))
          let (argStr, ty2) = peelFV id_ ty1
          let anns' = annotate id_ argStr anns
          let ty3 = modifyArgs (consArgStr argStr) ty2
          let ty4 = applyWhen (arity == 0) lazifyStrType ty3
          pure (mkStrLattice ty4 anns')
    CaseF scrut bndr _ alts -> do
      let transferAlt (_, bndrs, transfer) = do
            latt <- transfer arity
            pure (peelAndAnnotateFVs bndrs latt)
      StrLattice altTy altAnns <-
        peelAndAnnotateFV bndr . joins <$> mapM transferAlt alts
      StrLattice scrutTy scrutAnns <- scrut 0
      pure (mkStrLattice (scrutTy `bothStrType` altTy) (scrutAnns \/ altAnns))
    LetF bind body -> do
      let transferBinder (StrLattice ty anns) (id_, transfer) = do
            -- The final call to 'transfer' is only for annotations.
            -- Strictness on free variables was unleashed
            -- at call sites, now we only have to
            -- 'transfer' with manifest arity, which is accessible via
            -- 'idArity'.
            let (str, ty') = peelFV id_ ty
            let anns' = annotate id_ str anns
            StrLattice _ rhsAnns <- transfer $ Arity $ idArity id_
            pure (mkStrLattice ty' (anns' \/ rhsAnns))
      latt <- body arity
      foldM transferBinder latt (flattenBindsF [bind])
