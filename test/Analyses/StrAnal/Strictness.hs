{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Analyses.StrAnal.Strictness where

import           Algebra.Lattice
import           Algebra.Lattice.Lifted
import           Data.IntMap.Strict     (IntMap)
import           Unsafe.Coerce          (unsafeCoerce)

import           BasicTypes
import           Coercion
import           CoreArity
import           Id
import           Type
import           UniqFM
import           VarEnv

instance Show v => Show (UniqFM v) where
  -- I'd rather use coerce or the UFM constructor, but
  -- that isn't exported.
  show env = show (unsafeCoerce env :: IntMap v)

data Strictness
  = Lazy         -- ^ Evaluated lazily
  | Strict Arity -- ^ Evaluated strictly, called with 'n' args
  deriving Eq

instance Show Strictness where
  show Lazy       = "L"
  show (Strict n) = "S(" ++ show n ++ ")"

instance JoinSemiLattice Strictness where
  Lazy \/ _ = Lazy
  _ \/ Lazy = Lazy
  Strict n \/ Strict m = Strict (min n m)

instance MeetSemiLattice Strictness where
  Lazy /\ s = s
  s /\ Lazy = s
  Strict n /\ Strict m = Strict (max n m)

trimLiftedStrToType :: Type -> Lifted Strictness -> Strictness
trimLiftedStrToType ty Bottom   = Strict (length (typeArity ty))
trimLiftedStrToType ty (Lift s) = s

newtype StrEnv
  = StrEnv (VarEnv Strictness)
  deriving (Eq, Show)

instance JoinSemiLattice StrEnv where
  (StrEnv a) \/ (StrEnv b) = StrEnv (intersectUFM_C (\/) a b)

instance BoundedJoinSemiLattice StrEnv where
  bottom = StrEnv emptyVarEnv

unitStrEnv :: Var -> Strictness -> StrEnv
unitStrEnv id_ str = StrEnv (unitVarEnv id_ str)

bothStrEnv :: StrEnv -> StrEnv -> StrEnv
bothStrEnv (StrEnv a) (StrEnv b) =
  StrEnv (plusVarEnv_C (/\) a b)

peelStrEnv :: Id -> StrEnv -> (Lifted Strictness, StrEnv)
peelStrEnv id_ (StrEnv env) =
  (maybe Bottom Lift (lookupVarEnv env id_), StrEnv (delVarEnv env id_))

peelFV :: Id -> StrType -> (Lifted Strictness, StrType)
peelFV id_ ty =
  let (str, fvs') = peelStrEnv id_ (fvs ty)
  in (str, StrType (fvs', args ty))

lazifyStrEnv :: StrEnv -> StrEnv
lazifyStrEnv (StrEnv env) = StrEnv (mapVarEnv (const Lazy) env)

data ArgStr
  = BottomArgStr
  | TopArgStr
  | ConsArgStr !Strictness !ArgStr
  deriving Eq

instance Show ArgStr where
  show args = "[" ++ impl args ++ "]"
    where
      impl BottomArgStr           = "B,B.."
      impl TopArgStr              = "L,L.."
      impl (ConsArgStr str args') = show str ++ "," ++ impl args'

instance JoinSemiLattice ArgStr where
  BottomArgStr \/ s = s
  s \/ BottomArgStr = s
  TopArgStr \/ _ = TopArgStr
  _ \/ TopArgStr = TopArgStr
  (ConsArgStr s1 a1) \/ (ConsArgStr s2 a2) = ConsArgStr (s1 \/ s2) (a1 \/ a2)

instance BoundedJoinSemiLattice ArgStr where
  bottom = BottomArgStr

takesNoArgs :: ArgStr
takesNoArgs = TopArgStr

consArgStr :: Strictness -> ArgStr -> ArgStr
consArgStr = ConsArgStr

unconsArgStr :: ArgStr -> (Lifted Strictness, ArgStr)
unconsArgStr BottomArgStr     = (bottom, BottomArgStr)
unconsArgStr TopArgStr        = (Lift Lazy, TopArgStr)
unconsArgStr (ConsArgStr s a) = (Lift s, a)

newtype StrType
  = StrType (StrEnv, ArgStr)
  deriving (Eq, Show, JoinSemiLattice, BoundedJoinSemiLattice)

fvs :: StrType -> StrEnv
fvs (StrType (env, _)) = env

args :: StrType -> ArgStr
args (StrType (_, a)) = a

overFVs :: (StrEnv -> (a, StrEnv)) -> StrType -> (a, StrType)
overFVs f ty =
  let (a, fvs') = f (fvs ty)
  in (a, StrType (fvs', args ty))

overArgs :: (ArgStr -> (a, ArgStr)) -> StrType -> (a, StrType)
overArgs f ty =
  let (a, args') = f (args ty)
  in (a, StrType (fvs ty, args'))

modifyArgs :: (ArgStr -> ArgStr) -> StrType -> StrType
modifyArgs f = snd . overArgs (\a -> ((), f a))

emptyStrType :: StrType
emptyStrType = StrType (bottom, takesNoArgs)

unitStrType :: Id -> Strictness -> StrType
unitStrType id_ str = StrType (unitStrEnv id_ str, takesNoArgs)

-- | Sequential composition, or Par or both.
-- This is right biased, meaning that it will return the
-- argument strictness of the right argument.
bothStrType :: StrType -> StrType -> StrType
bothStrType (StrType (fvs1, _)) (StrType (fvs2, args2)) =
  StrType (bothStrEnv fvs1 fvs2, args2)

lazifyStrType :: StrType -> StrType
lazifyStrType ty = StrType (fvs', args ty)
  -- Doesn't change argument strictness, but
  -- it shouldn't actually matter.
  -- Anyway, ArgStr always corresponds to a
  -- single incoming call.
  where
    fvs' = lazifyStrEnv (fvs ty)

coercionStrType :: Coercion -> StrType
coercionStrType co = StrType (fv, takesNoArgs)
  where
    fv = StrEnv (mapVarEnv (const Lazy) (coVarsOfCo co))

newtype Annotations
  = Ann (VarEnv Strictness)
  deriving (Eq, Show)

emptyAnnotations :: Annotations
emptyAnnotations = Ann emptyVarEnv

overwriteError :: (Show a, Show b) => a -> b -> c
overwriteError old new =
  error $
    "Should never overwrite an annotation. Old: "
    ++ show old ++ ", New: "
    ++ show new

instance JoinSemiLattice Annotations where
  (Ann a) \/ (Ann b) = Ann $ plusVarEnv_C overwriteError a b

instance BoundedJoinSemiLattice Annotations where
  bottom = emptyAnnotations

annotate :: Id -> Strictness -> Annotations -> Annotations
annotate id_ str (Ann anns) = Ann (extendVarEnv_C overwriteError anns id_ str)

lookupAnnotation :: Id -> Annotations -> Maybe Strictness
lookupAnnotation id_ (Ann env) = lookupVarEnv env id_

newtype StrLattice
  = StrLattice (StrType, Annotations)
  deriving (Eq, Show, JoinSemiLattice, BoundedJoinSemiLattice)

mkStrLattice :: StrType -> Annotations -> StrLattice
mkStrLattice ty ann = StrLattice (ty, ann)

emptyStrLattice :: StrLattice
emptyStrLattice = mkStrLattice emptyStrType emptyAnnotations

strType :: StrLattice -> StrType
strType (StrLattice (ty, _)) = ty

annotations :: StrLattice -> Annotations
annotations (StrLattice (_, anns)) = anns

peelAndAnnotateFV :: Id -> StrLattice -> StrLattice
peelAndAnnotateFV id_ (StrLattice (ty, anns)) =
  let (liftedStr, ty') = peelFV id_ ty
      str = trimLiftedStrToType (idType id_) liftedStr
      anns' = annotate id_ str anns
  in mkStrLattice ty' anns'

peelAndAnnotateFVs :: [Id] -> StrLattice -> StrLattice
peelAndAnnotateFVs ids latt = foldr peelAndAnnotateFV latt ids
