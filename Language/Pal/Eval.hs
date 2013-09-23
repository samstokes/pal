{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Pal.Eval
  ( eval
  , initialEnv
  ) where

import Control.Applicative
import Control.Error
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.State
import Data.Monoid ((<>))

import Language.Pal.Types


newtype EvalT m a = EvalT { unEvalT :: EitherT EvalError (StateT Env m) a }
  deriving (Monad, MonadError EvalError, MonadState Env)

instance MonadTrans EvalT where
  lift = EvalT . lift . lift

unpackStateT :: EvalT m a -> StateT Env m (Either EvalError a)
unpackStateT = runEitherT . unEvalT

runEvalT :: Monad m => EvalT m a -> Env -> m (Either EvalError a, Env)
runEvalT = runStateT . unpackStateT

evalEvalT :: Monad m => EvalT m a -> Env -> m (Either EvalError a)
evalEvalT = evalStateT . unpackStateT

localEnv :: Monad m => (Env -> Env) -> EvalT m a -> EvalT m a
localEnv f e = do
  modifiedEnv <- gets f
  eOrV <- lift $ evalEvalT e modifiedEnv
  liftEither eOrV

liftEither :: Monad m => Either EvalError a -> EvalT m a
liftEither = EvalT . hoistEither


eval :: (Applicative m, Monad m) => LValue -> Env -> m (Either EvalError LValue, Env)
eval val = runEvalT (eval' val)

eval' :: (Applicative m, Monad m) => LValue -> EvalT m LValue
eval' v@(Number _) = return v
eval' v@(String _) = return v
eval' v@(Bool _) = return v
eval' (List l) = evalForm l
eval' (Atom name) = atom name
eval' v@(BuiltinFunction _) = return v
eval' v@(LispFunction _) = return v

evalForm :: (Applicative m, Monad m) => LList -> EvalT m LValue
evalForm [Atom "quote", e] = return e
evalForm [Atom "set!", Atom v, e] = do
  rval <- eval' e
  modify (setAtom v rval)
  return rval
evalForm [Atom "if", condExpr, thenExpr, elseExpr] = do
  cond <- eval' condExpr
  b <- liftEither $ coerceBool cond
  if b then eval' thenExpr else eval' elseExpr
evalForm (Atom "lambda" : List params : body) = do
  paramNames <- liftEither $ mapM coerceAtom params
  scope <- get
  return $ LispFunction LLispFunction {
      lfScope = scope
    , lfParams = paramNames
    , lfBody = body
    }
evalForm (funExp : argExps) = do
  fun <- eval' funExp
  args <- mapM eval' argExps
  apply fun args
evalForm [] = throwError "can't eval empty list"


apply :: (Applicative m, Monad m) => LValue -> [LValue] -> EvalT m LValue
apply (BuiltinFunction (LBuiltinFunction _ f)) args = liftEither $ f args
apply (LispFunction (LLispFunction scope params body)) args = do
    _ <- liftEither $ params `checkSameLength` args
    localEnv (const fnScope) $ foldM (\_ e -> eval' e) nil body
  where
    fnScope = argBindings <> scope
    argBindings = Env $ zip params args
apply v _ = throwError $ "not a function: " ++ show v


atom :: (Applicative m, Monad m) => LAtom -> EvalT m LValue
atom name = EvalT $ do
  env <- get
  lookupAtom name env ?? ("not found: " ++ name)


builtin :: LAtom -> TFunction -> (LAtom, LValue)
builtin name f = (name, BuiltinFunction (LBuiltinFunction name f))


data Tag =
    TagSymbol
  | TagList
  | TagNumber
  | TagString
  | TagBool
  | TagFunction
  deriving (Eq)

instance Show Tag where
  show TagSymbol = "symbol"
  show TagList = "list"
  show TagNumber = "number"
  show TagString = "string"
  show TagBool = "boolean"
  show TagFunction = "function"

tag :: LValue -> Tag
tag (Atom _) = TagSymbol
tag (List _) = TagList
tag (Number _) = TagNumber
tag (String _) = TagString
tag (Bool _) = TagBool
tag (BuiltinFunction _) = TagFunction
tag (LispFunction _) = TagFunction

builtinTagPred :: Tag -> (LAtom, LValue)
builtinTagPred t = (name, BuiltinFunction (LBuiltinFunction name p)) where
  name = show t ++ "?"
  p = unop ((== t) . tag) Bool return

check :: Tag -> LValue -> Either EvalError LValue
check t v | tag v == t = Right v
          | otherwise = throwError $ "expected " ++ show t ++ ", got " ++ show (tag v)

coerceAtom :: LValue -> Either EvalError LAtom
coerceAtom = fmap lvAtom . check TagSymbol
coerceNumber :: LValue -> Either EvalError LNumber
coerceNumber = fmap lvNumber . check TagNumber
coerceString :: LValue -> Either EvalError LString
coerceString = fmap lvString . check TagString
coerceBool :: LValue -> Either EvalError Bool
coerceBool = fmap lvBool . check TagBool


checkN :: Int -> [a] -> Either EvalError [a]
checkN n = fmap (take n) . flip checkSameLength (replicate n undefined)

unop :: (a -> b) -> (b -> LValue) -> (LValue -> Either EvalError a) -> [LValue] -> Either EvalError LValue
unop f outputCtor inputCoercion = fmap (outputCtor . f) . inputCoercion <=< fmap head . checkN 1

varop :: ([a] -> b) -> (b -> LValue) -> (LValue -> Either EvalError a) -> [LValue] -> Either EvalError LValue
varop f outputCtor inputCoercion = fmap (outputCtor . f) . mapM inputCoercion


checkSameLength :: [a] -> [b] -> Either EvalError [a]
checkSameLength actual expected
    | nAct == nExp = Right actual
    | nAct <  nExp = Left $ "not enough arguments: expected " ++ show nExp ++ ", got " ++ show nAct
    | otherwise    = Left $ "too many arguments: expected " ++ show nExp ++ ", got " ++ show nAct
        where nAct = length actual; nExp = length expected


initialEnv :: Env
initialEnv = Env $ map (uncurry builtin) [
    ("+", varop sum Number coerceNumber)
  , ("concat", varop concat String coerceString)
  , ("typeof", unop (show . tag) String return)
  ] ++ map builtinTagPred [
    TagSymbol
  , TagList
  , TagNumber
  , TagString
  , TagBool
  , TagFunction
  ]
