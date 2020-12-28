module Analyses.AdHocStrAnal (analyse) where

import           Datafix.SemiLattice
import           Analyses.StrAnal.Arity
import           Analyses.StrAnal.Strictness

import           CoreSyn
import           CoreArity
import           Id
import           Var
import           VarEnv

analyse :: CoreExpr -> StrLattice
analyse e = analExpr emptyVarEnv e 0

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f  = f
applyWhen False _ = id

type SigEnv = VarEnv (Arity, StrType)

analExpr :: SigEnv -> CoreExpr -> Arity -> StrLattice
analExpr env expr arity =
  case expr of
    Lit _       -> emptyStrLattice
    Type _      -> emptyStrLattice
    -- Coercions are irrelevant to Strictness Analysis:
    -- 'emptyStrLattice' is already the 'top' element,
    -- so it's a safe approximation.
    Coercion _ -> emptyStrLattice
    Tick _ e    -> analExpr env e arity
    Cast e _ -> analExpr env e arity
    App f a ->
      let
        StrLattice fTy fAnns = analExpr env f (arity + 1)
        (argStr, fTy') = overArgs unconsArgStr fTy
        argArity =
          case argStr of
            -- It's unfortunate that we don't have the type available to
            -- trim this... But it doesn't hurt either.
            HyperStrict -> Arity maxBound
            Lazy        -> 0
            Strict n    -> n
        StrLattice aTy aAnns = analExpr env a argArity
      in mkStrLattice (aTy `bothStrType` fTy') (fAnns \/ aAnns)
    Var id_
      | isLocalId id_ ->
          let
            rhsType = case lookupVarEnv env id_ of
              Just (sig_arity, ty)
                -- 'ty' is a safe approximation for a call with at least sig_arity.
                -- Note that 'Arity' is 'Op' ordered.
                | arity <= sig_arity -> ty
              _ -> emptyStrType
          in mkStrLattice (unitStrType id_ (Strict arity) `bothStrType` rhsType) emptyAnnotations
      | otherwise -> emptyStrLattice
    Lam id_ body
      | isTyVar id_ -> analExpr env body arity
      | otherwise ->
          let
            StrLattice ty1 anns = analExpr env body (0 /\ (arity-1))
            (argStr, ty2) = peelFV id_ ty1
            anns' = annotate id_ argStr anns
            ty3 = modifyArgs (consArgStr argStr) ty2
            ty4 = applyWhen (arity == 0) lazifyStrType ty3
          in mkStrLattice ty4 anns'
    Case scrut bndr _ alts ->
      let
        transferAlt (_, bndrs, alt) =
          peelAndAnnotateFVs bndrs (analExpr env alt arity)
        StrLattice altTy altAnns =
          peelAndAnnotateFV bndr . joins . map transferAlt $ alts
        StrLattice scrutTy scrutAnns = analExpr env scrut 0
      in mkStrLattice (scrutTy `bothStrType` altTy) (scrutAnns \/ altAnns)
    Let bind body ->
      let
        -- we assume a single call with manifest arity for our approximation
        (rhsAnns, env') = case bind of
          NonRec id_ rhs
            | let sig_arity = Arity $ manifestArity rhs
            , StrLattice ty anns <- analExpr env rhs arity
            -> (anns, extendVarEnv env id_ (sig_arity, ty))
          Rec binds  -> fixBinds env binds
        bodyLatt = analExpr env' body arity
        StrLattice bodyTy bodyAnns = peelAndAnnotateFVs (bindersOf bind) bodyLatt
      in mkStrLattice bodyTy (bodyAnns \/ rhsAnns)

fixBinds :: SigEnv -> [(Id, CoreExpr)] -> (Annotations, SigEnv)
fixBinds env binds = mergeWithLatts stableLatts
  where
    mergeWithLatts :: [StrLattice] -> (Annotations, SigEnv)
    mergeWithLatts latts = foldr merger (emptyAnnotations, env) (zip binds latts)

    merger :: ((Id, CoreExpr), StrLattice) -> (Annotations, SigEnv) -> (Annotations, SigEnv)
    merger ((id_, rhs), StrLattice ty anns) (restAnns, env') =
      (restAnns \/ anns, extendVarEnv env' id_ (Arity $ manifestArity rhs, ty))

    latts0 :: [StrLattice]
    latts0 = map (const bottom) binds

    approximations :: [[StrLattice]]
    approximations = iterate (iter . snd . mergeWithLatts) latts0

    stable :: ([StrLattice], [StrLattice]) -> Bool
    stable (old, new) = map strTy old == map strTy new

    stableLatts :: [StrLattice]
    stableLatts = snd . head . filter stable $ zip approximations (tail approximations)

    iter env' = snd (foldr iterBind (env', []) binds)

    iterBind (id_, rhs) (env', latts) =
      let
        arity = Arity $ manifestArity rhs
        latt = analExpr env' rhs arity
        env'' = extendVarEnv env' id_ (arity, strTy latt)
      in (env'', latt:latts)
