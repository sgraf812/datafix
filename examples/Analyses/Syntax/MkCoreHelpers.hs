module Analyses.Syntax.MkCoreHelpers where

import           CoreSyn
import           FastString
import           Id
import           Literal
import           MkCore
import           Type
import           TysWiredIn
import           Unique

mkTestId :: Int -> String -> Type -> Id
mkTestId i s = mkSysLocal (mkFastString s) (mkBuiltinUnique i)

mkTestIds :: [(String, Type)] -> [Id]
mkTestIds = zipWith (\i (s, t) -> mkTestId i s t) [0..]

int :: Type
int = intTy

bool :: Type
bool = boolTy

int2int :: Type
int2int = mkFunTys [int] int

int2int2int :: Type
int2int2int = mkFunTys [int, int] int

bool2int2int :: Type
bool2int2int = mkFunTys [bool, int] int

letrec :: Id -> CoreExpr -> CoreExpr -> CoreExpr
letrec id_ rhs = mkCoreLet (Rec [(id_, rhs)])

lam :: Id -> CoreExpr -> CoreExpr
lam = Lam

var :: Id -> CoreExpr
var = Var

ite :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
ite = mkIfThenElse

($$) :: CoreExpr -> CoreExpr -> CoreExpr
f $$ a = App f a

intLit :: Integer -> CoreExpr
intLit i = Lit (mkLitInteger i int)

boolLit :: Bool -> CoreExpr
boolLit True  = Var trueDataConId
boolLit False = Var falseDataConId
