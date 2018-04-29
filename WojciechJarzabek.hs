{-# LANGUAGE Safe #-}

module WojciechJarzabek (typecheck, eval) where

import AST
import DataTypes
import qualified Data.Map as Map

data Val p = VNum Integer | VBool Bool | VUnit | VPair (Val p) (Val p) | VCons (Val p) (Val p) | VNil | VCloLocal (Env (Val p)) Var (Expr p) | VCloGlobal [FunctionDef p] (FunctionDef p)

type Error p = (p, ErrKind)
data ErrKind = EUndefinedVariable Var | EUndefinedFunction Var
             | ETypeMismatch Type Type | EBranchMismatch Type Type
             | EPairMismatch Type | EListMismatch Type | EOldTest
type Env a = [(Var, a)]
type IRes p = Either (Error p) Type

instance Show ErrKind where
  show (EUndefinedVariable x)  =
    "Undefined variable " ++ show x ++ "."
  show (EUndefinedFunction x)  =
    "Undefined function " ++ show x ++ "."
  show (ETypeMismatch t1 t2)   =
    "Type mismatch: expected " ++ show t1 ++ " but received " ++ show t2 ++ "."
  show (EBranchMismatch t1 t2) =
    "Type mismatch in the branches: " ++ show t1 ++ " and " ++ show t2 ++ "."
  show (EPairMismatch t)       =
    "Type mismatch: expected a pair, but received " ++ show t ++ "."
  show (EListMismatch t)       =
    "Type mismatch: expected a list, but received " ++ show t ++ "."

infixr 6 $>

($>) :: Maybe a -> Either a b -> Either a b
Just e  $> _ = Left e
Nothing $> e = e


inferType :: Env Type -> Expr p -> IRes p

inferType γ (EVar p x) =
  case lookup x γ of
    Just t  -> Right t
    Nothing -> Left (p, EUndefinedVariable x)
    
inferType γ (ENum _ _)  = Right TInt

inferType γ (EBool _ _) = Right TBool
inferType γ (EUnary _ op e) =
  checkType γ e ta $>
  Right tr
  where (ta, tr) = uopType op
  
inferType γ (EBinary _ op e1 e2) =
  checkType γ e1 et1 $>
  checkType γ e2 et2 $>
  Right tr
  where (et1, et2, tr) = bopType op
  
inferType γ (ELet _ x ex eb) =
  case inferType γ ex of
    Left err -> Left err
    Right tx -> inferType ((x, tx) : γ) eb
    
inferType γ (EIf p ec et ef) =
  checkType γ ec TBool $>
  checkEqual p (inferType γ et) (inferType γ ef)
  
inferType γ (EApp p fa e) =
  case fa of
    (EVar p' f) -> case inferType γ e of
        Right tpe -> case lookup f γ of
          Just tpf -> case tpf of
              (TArrow _ _) -> checkType ((f, tpf):γ) e (hlpr1 tpf) $> Right (hlpr2 tpf)
              _ -> Left (p, EOldTest)
          Nothing -> Left (p, EUndefinedFunction f)
        Left err -> Left err
    x -> inferType γ e
  where hlpr1 (TArrow x _) = x
        hlpr2 (TArrow _ y) = y
    
inferType γ (EUnit _) = Right TUnit

inferType γ (EPair _ e1 e2) =
  case (inferType γ e1, inferType γ e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right t1, Right t2) -> Right $ TPair t1 t2
    
inferType γ (EFst p e) =
  fmap fst . checkPair p $ inferType γ e
  
inferType γ (ESnd p e) =
  fmap snd . checkPair p $ inferType γ e
  
inferType γ (ENil p t) =
  fmap TList . checkList p $ Right t
  
inferType γ (ECons p eh et) =
  fmap TList $ checkEqual p (inferType γ eh) (checkList (getData et) $ inferType γ et)
  
inferType γ (EMatchL p e en (x, xs, ec)) =
  case checkList (getData e) $ inferType γ e of
    Right t  -> checkEqual p (inferType γ en) (inferType ((x, t):(xs, TList t):γ) ec)
    Left err -> Left err
    
inferType γ (EFn _ nm tp e) = 
  case inferType ((nm, tp):γ) e of
      Right a -> Right (TArrow tp a)
      Left err -> Left err
          
checkPair :: p -> IRes p -> Either (Error p) (Type, Type)
checkPair _ (Right (TPair t1 t2)) = Right (t1, t2)
checkPair p (Right t)  = Left $ (p, EPairMismatch t)
checkPair _ (Left err) = Left err

checkList :: p -> IRes p -> IRes p
checkList _ (Right (TList t)) = Right t
checkList p (Right t)  = Left $ (p, EListMismatch t)
checkList _ (Left err) = Left err

checkEqual :: p -> IRes p -> IRes p -> IRes p
checkEqual _ (Left err) _ = Left err
checkEqual _ _ (Left err) = Left err
checkEqual p (Right t1) (Right t2) =
  if t1 == t2 then Right t1
  else Left (p, EBranchMismatch t1 t2)

checkType :: Env Type -> Expr p -> Type -> Maybe (Error p)
checkType γ e t =
  case inferType γ e of
    Left err -> Just err
    Right t' -> if t == t' then Nothing else Just (getData e, ETypeMismatch t' t)

uopType :: UnaryOperator -> (Type, Type)
uopType UNot = (TBool, TBool)
uopType UNeg = (TInt,  TInt)

bopType e = case e of
  BAnd -> tbool
  BOr  -> tbool
  BEq  -> tcomp
  BNeq -> tcomp
  BLt  -> tcomp
  BLe  -> tcomp
  BGt  -> tcomp
  BGe  -> tcomp
  BAdd -> tarit
  BSub -> tarit
  BMul -> tarit
  BDiv -> tarit
  BMod -> tarit
  where tbool = (TBool, TBool, TBool)
        tcomp = (TInt,  TInt,  TBool)
        tarit = (TInt,  TInt,  TInt)


---------------------------------------------------------------------


typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck fs vs e = 
  case maybe (checkType (φ ++ γ) e TInt) Just $ foldl comb Nothing fs of
    Nothing  -> Ok
    Just (p, err) -> Error p $ show err
  where γ = map (\ x -> (x, TInt)) vs
        φ = map (\ f -> (funcName f, TArrow (funcArgType f) (funcResType f))) fs
        checkFD f = checkType ([(funcArg f, funcArgType f)] ++ γ ++ φ) (funcBody f) (funcResType f)
        comb a f = maybe (checkFD f) Just a 


---------------------------------------------------------------------

ev :: Env (Val p) -> Expr p -> Maybe (Val p)

ev φ (EVar _ x)  = lookup x φ

ev φ (ENum _ n)  = Just $ VNum n

ev φ (EBool _ b) = Just $ VBool b

ev φ (EUnary _ op e) =
  case ev φ e of
    Just v -> evUOp op v
    Nothing -> Nothing
    
ev φ (EBinary _ op e1 e2) =
  case (ev φ e1, ev φ e2) of
    (Just v1, Just v2) -> evBOp op v1 v2
    _ -> Nothing
    
ev φ (ELet _ x ex eb) =
  case ev φ ex of
    Just v -> ev ((x, v):φ) eb
    Nothing -> Nothing
    
ev φ (EIf _ ec et ef) =
  case ev φ ec of
    Just (VBool True)  -> ev φ et
    Just (VBool False) -> ev φ ef
    _ -> Nothing
    
ev φ (EApp p fn e) = 
  case (ev φ fn, ev φ e) of
    (_, Nothing) -> Nothing
    (Just (VCloLocal env var expr), Just val) -> ev ((var,val):env) expr
    (Just (VCloGlobal env f), Just val) -> ev (((funcArg f),val):(conc env [])) (funcBody f)
  where conc a b = zip (Map.keys (convert a b)) (Map.elems (convert a b))
        convert fs list = Map.union (hlpr1 list) (hlpr2 fs fs)
        hlpr1 [] = Map.empty
        hlpr1 ((x,int):xs) = Map.insert x (VNum int) (hlpr1 xs)
        hlpr2 [] _ = Map.empty
        hlpr2 (f:fs) funl = Map.insert (funcName f) (VCloGlobal funl f) (hlpr2 fs funl)
    
ev φ (EUnit _) = Just VUnit

ev φ (EPair _ e1 e2) =
  case (ev φ e1, ev φ e2) of
    (Just v1, Just v2) -> Just $ VPair v1 v2
    _ -> Nothing
    
ev φ (EFst _ e) =
  case ev φ e of
    Just (VPair v _) -> Just v
    Nothing -> Nothing
    
ev φ (ESnd _ e) =
  case ev φ e of
    Just (VPair _ v) -> Just v
    Nothing -> Nothing
    
ev φ (ENil _ _) = Just VNil

ev φ (ECons _ eh et) =
  case (ev φ eh, ev φ et) of
    (Just vh, Just vt) -> Just $ VCons vh vt
    _ -> Nothing
    
ev φ (EMatchL _ e en (x, xs, ec)) =
  case ev φ e of
    Just VNil -> ev φ en
    Just (VCons vh vt) -> ev ((x, vh):(xs, vt):φ) ec
    Nothing -> Nothing
    
ev φ (EFn p nm tp e) = 
  Just (VCloLocal φ nm e)

evUOp UNot (VBool b) = Just . VBool $ not b
evUOp UNeg (VNum n)  = Just . VNum $ -n

evBOp BAnd (VBool b1) (VBool b2) = Just . VBool $ b1 && b2
evBOp BOr  (VBool b1) (VBool b2) = Just . VBool $ b1 || b2
evBOp BEq  (VNum n1)  (VNum n2)  = Just . VBool $ n1 == n2
evBOp BNeq (VNum n1)  (VNum n2)  = Just . VBool $ n1 /= n2
evBOp BLt  (VNum n1)  (VNum n2)  = Just . VBool $ n1 <  n2
evBOp BLe  (VNum n1)  (VNum n2)  = Just . VBool $ n1 <= n2
evBOp BGt  (VNum n1)  (VNum n2)  = Just . VBool $ n1 >  n2
evBOp BGe  (VNum n1)  (VNum n2)  = Just . VBool $ n1 >= n2
evBOp BAdd (VNum n1)  (VNum n2)  = Just . VNum  $ n1 + n2
evBOp BSub (VNum n1)  (VNum n2)  = Just . VNum  $ n1 - n2
evBOp BMul (VNum n1)  (VNum n2)  = Just . VNum  $ n1 * n2
evBOp BDiv (VNum n1)  (VNum n2)
  | n2 == 0   = Nothing
  | otherwise = Just . VNum  $ n1 `div` n2
evBOp BMod (VNum n1)  (VNum n2)
  | n2 == 0   = Nothing
  | otherwise = Just . VNum  $ n1 `mod` n2


---------------------------------------------------------------------


eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval fs args e =
  case ev (σ ++ φ) e of
    Just (VNum n) -> Value n
    Just _        -> undefined
    Nothing       -> RuntimeError
  where σ = (map (\(x, n) -> (x, VNum n)) args)
        φ = (map (\f -> (funcName f, VCloGlobal fs f)) fs)
        


