{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Analyses.StrAnal.Strictness where

import           Datafix.SemiLattice
import           Data.Maybe             (fromMaybe)

import           Analyses.StrAnal.Arity

import           Coercion
import           Id
import           UniqFM
import           VarEnv

instance Show v => Show (UniqFM v) where
  show = show . nonDetUFMToList

-- | Captures lower bounds on evaluation cardinality of some variable.
-- E.g.: Is this variable evaluated at least once, and if so, what is the
-- maximum number of arguments it was surely applied to?
data Strictness
  = Lazy         -- ^ Evaluated lazily (possibly not evaluated at all)
  | Strict Arity -- ^ Evaluated strictly (>= 1), called with n args
  | HyperStrict  -- ^ Fully evaluated, a call with maximum arity
  deriving Eq

instance Show Strictness where
  show Lazy        = "L"
  show (Strict n)  = "S(" ++ show n ++ ")"
  show HyperStrict = "B"

instance JoinSemiLattice Strictness where
  Lazy \/ _ = Lazy
  _ \/ Lazy = Lazy
  HyperStrict \/ s = s
  s \/ HyperStrict = s
  Strict n \/ Strict m = Strict (n \/ m)

instance BoundedJoinSemiLattice Strictness where
  bottom = HyperStrict

instance MeetSemiLattice Strictness where
  HyperStrict /\ _ = HyperStrict
  _ /\ HyperStrict = HyperStrict
  Lazy /\ s = s
  s /\ Lazy = s
  Strict n /\ Strict m = Strict (n /\ m)

instance BoundedMeetSemiLattice Strictness where
  top = Lazy

-- | Captures certain divergence through 'Diverges', which allows
-- to assume a 'defaultStr' of 'HyperStrict'.
data Termination
  = Diverges -- ^ Denotes certain divergence
  | Dunno    -- ^ Possibly terminates
  deriving (Eq, Show)

instance JoinSemiLattice Termination where
  Diverges \/ t = t
  t \/ Diverges = t
  _ \/ _ = Dunno

instance BoundedJoinSemiLattice Termination where
  bottom = Diverges

instance MeetSemiLattice Termination where
  Diverges /\ _ = Diverges
  _ /\ Diverges = Diverges
  _ /\ _ = Dunno

instance BoundedMeetSemiLattice Termination where
  top = Dunno

-- | In the case of divergence, we want to assume
-- an optimistic 'HyperStrict' strictness for any variables
-- not present in the 'StrEnv'.
-- Otherwise, those variables are possible absent and thus used
-- lazily.
defaultStr :: Termination -> Strictness
defaultStr Dunno    = Lazy
defaultStr Diverges = HyperStrict

-- | Tracks strictness on free variables of a possibly diverging
-- expression.
data StrEnv
  = StrEnv !(VarEnv Strictness) !Termination
  deriving Eq

instance Show StrEnv where
  show (StrEnv env Dunno)    = show env
  show (StrEnv env Diverges) = show env ++ "b"

instance JoinSemiLattice StrEnv where
  (StrEnv a t1) \/ (StrEnv b t2) =
    StrEnv (plusVarEnv_CD (\/) a (defaultStr t1) b (defaultStr t2)) (t1 \/ t2)

instance BoundedJoinSemiLattice StrEnv where
  bottom = StrEnv emptyVarEnv Diverges

instance MeetSemiLattice StrEnv where
  (StrEnv a t1) /\ (StrEnv b t2) =
    StrEnv (plusVarEnv_CD (/\) a (defaultStr t1) b (defaultStr t2)) (t1 /\ t2)

instance BoundedMeetSemiLattice StrEnv where
  top = StrEnv emptyVarEnv Dunno

unitStrEnv :: Var -> Strictness -> StrEnv
unitStrEnv id_ str = StrEnv (unitVarEnv id_ str) Dunno

peelStrEnv :: Id -> StrEnv -> (Strictness, StrEnv)
peelStrEnv id_ (StrEnv env t) =
  (fromMaybe (defaultStr t) (lookupVarEnv env id_), StrEnv (delVarEnv env id_) t)

peelFV :: Id -> StrType -> (Strictness, StrType)
peelFV id_ ty =
  let (str, fvs') = peelStrEnv id_ (fvs ty)
  in (str, ty { fvs = fvs' })

lazifyStrEnv :: StrEnv -> StrEnv
lazifyStrEnv _ = top

data ArgStr
  = BottomArgStr
  | TopArgStr
  | ConsArgStr !Strictness !ArgStr
  deriving Eq

instance Show ArgStr where
  show argStr = "[" ++ impl argStr ++ "]"
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

-- | This instance doesn't make a lot of sense semantically,
-- but it's the dual to the 'JoinSemiLattice' instance.
-- We mostly need this for 'top'.
instance MeetSemiLattice ArgStr where
  BottomArgStr /\ _ = BottomArgStr
  _ /\ BottomArgStr = BottomArgStr
  TopArgStr /\ s = s
  s /\ TopArgStr = s
  (ConsArgStr s1 a1) /\ (ConsArgStr s2 a2) = ConsArgStr (s1 /\ s2) (a1 /\ a2)

instance BoundedMeetSemiLattice ArgStr where
  top = TopArgStr

consArgStr :: Strictness -> ArgStr -> ArgStr
consArgStr Lazy TopArgStr           = TopArgStr
consArgStr HyperStrict BottomArgStr = BottomArgStr
consArgStr s a                      = ConsArgStr s a

unconsArgStr :: ArgStr -> (Strictness, ArgStr)
unconsArgStr BottomArgStr     = (bottom, BottomArgStr)
unconsArgStr TopArgStr        = (top, TopArgStr)
unconsArgStr (ConsArgStr s a) = (s, a)

data StrType
  = StrType
  { fvs  :: !StrEnv
  , args :: !ArgStr
  } deriving (Eq, Show)

instance JoinSemiLattice StrType where
  (StrType fvs1 args1) \/ (StrType fvs2 args2) =
    StrType (fvs1 \/ fvs2) (args1 \/ args2)

instance BoundedJoinSemiLattice StrType where
  bottom = StrType bottom bottom

-- | This instance doesn't make a lot of sense semantically,
-- but it's the dual to the 'JoinSemiLattice' instance.
-- We mostly need this for 'top'.
instance MeetSemiLattice StrType where
  (StrType fvs1 args1) /\ (StrType fvs2 args2) =
    StrType (fvs1 /\ fvs2) (args1 /\ args2)

instance BoundedMeetSemiLattice StrType where
  top = StrType top top

overFVs :: (StrEnv -> (a, StrEnv)) -> StrType -> (a, StrType)
overFVs f ty =
  let (a, fvs') = f (fvs ty)
  in (a, ty { fvs = fvs' })

overArgs :: (ArgStr -> (a, ArgStr)) -> StrType -> (a, StrType)
overArgs f ty =
  let (a, args') = f (args ty)
  in (a, ty { args = args' })

modifyArgs :: (ArgStr -> ArgStr) -> StrType -> StrType
modifyArgs f = snd . overArgs (\a -> ((), f a))

emptyStrType :: StrType
emptyStrType = top

unitStrType :: Id -> Strictness -> StrType
unitStrType id_ str = StrType (unitStrEnv id_ str) top

-- | Sequential composition, or Par or both.
-- This is right biased, meaning that it will return the
-- argument strictness of the right argument.
bothStrType :: StrType -> StrType -> StrType
bothStrType (StrType fvs1 _) (StrType fvs2 args2) =
  StrType (fvs1 /\ fvs2) args2

lazifyStrType :: StrType -> StrType
lazifyStrType ty = StrType fvs' (args ty)
  -- Doesn't change argument strictness, but
  -- it shouldn't actually matter.
  -- Anyway, ArgStr always corresponds to a
  -- single incoming call.
  where
    fvs' = lazifyStrEnv (fvs ty)

-- | Tracks annotations in the syntax tree.
-- Has an instance of 'JoinSemiLattice', but
-- really doesn't allow overwriting annotations.
newtype Annotations
  = Ann (VarEnv Strictness)
  deriving (Eq, Show)

emptyAnnotations :: Annotations
emptyAnnotations = Ann emptyVarEnv

instance JoinSemiLattice Annotations where
  (Ann a) \/ (Ann b) = Ann $ plusVarEnv_C (\/) a b

instance BoundedJoinSemiLattice Annotations where
  bottom = emptyAnnotations

overwriteError :: (Show a, Show b) => a -> b -> c
overwriteError old new =
  error $
    "Should never overwrite an annotation. Old: "
    ++ show old ++ ", New: "
    ++ show new

annotate :: Id -> Strictness -> Annotations -> Annotations
annotate id_ str (Ann anns) = Ann (extendVarEnv_C overwriteError anns id_ str)

lookupAnnotation :: Id -> Annotations -> Maybe Strictness
lookupAnnotation id_ (Ann env) = lookupVarEnv env id_

data StrLattice
  = StrLattice
  { strTy :: !StrType
  , strAnns :: !Annotations
  } deriving (Eq, Show)

instance JoinSemiLattice StrLattice where
  StrLattice ty1 anns1 \/ StrLattice ty2 anns2 = StrLattice (ty1 \/ ty2) (anns1 \/ anns2)

instance BoundedJoinSemiLattice StrLattice where
  bottom = StrLattice bottom bottom

mkStrLattice :: StrType -> Annotations -> StrLattice
mkStrLattice ty ann = StrLattice ty ann

emptyStrLattice :: StrLattice
emptyStrLattice = mkStrLattice emptyStrType emptyAnnotations

peelAndAnnotateFV :: Id -> StrLattice -> StrLattice
peelAndAnnotateFV id_ (StrLattice ty anns) =
  let (str, ty') = peelFV id_ ty
      anns' = annotate id_ str anns
  in mkStrLattice ty' anns'

peelAndAnnotateFVs :: [Id] -> StrLattice -> StrLattice
peelAndAnnotateFVs ids latt = foldr peelAndAnnotateFV latt ids
