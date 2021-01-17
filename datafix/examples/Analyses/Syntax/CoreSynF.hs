module Analyses.Syntax.CoreSynF where

import           Coercion
import           CoreSyn
import           Id
import           Literal
import           Type
import           Unsafe.Coerce (unsafeCoerce)

newtype Fix f
  = In { out :: f (Fix f) }

type AltF b a = (AltCon, [b], a)

data BindF b a
  = NonRecF b a
  | RecF [(b, a)]

flattenBindsF :: [BindF b a] -> [(b, a)]
flattenBindsF = concatMap impl
  where
    impl (NonRecF b a) = [(b, a)]
    impl (RecF bs)     = bs

data ExprF b a
  = VarF Id
  | LitF Literal
  | AppF a a
  | LamF b a
  | LetF (BindF b a) a
  | CaseF a b Type [AltF b a]
  | CastF a Coercion
  | TickF (Tickish Id) a
  | TypeF Type
  | CoercionF Coercion

type CoreExprF
  = ExprF CoreBndr

-- | 'unsafeCoerce' mostly because I'm too lazy to write the boilerplate.
fromCoreExpr :: CoreExpr -> Fix CoreExprF
fromCoreExpr = unsafeCoerce

toCoreExpr :: CoreExpr -> Fix CoreExprF
toCoreExpr = unsafeCoerce
