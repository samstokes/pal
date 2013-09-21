{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Pal.Eval
  ( eval
  , Error
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Error
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.State

import Language.Pal.Types


newtype Env = Env { unEnv :: [(LAtom, LValue)] }
  deriving (Show)

type Error = String

newtype EvalT m a = EvalT { unEvalT :: EitherT Error (StateT Env m) a }
  deriving (Monad, MonadError Error)

runEvalT :: Monad m => EvalT m a -> Env -> m (Either Error a, Env)
runEvalT = runStateT . runEitherT . unEvalT


eval :: (Applicative m, Monad m) => LValue -> m (Either Error LValue, Env)
eval val = runEvalT (eval' val) initialEnv

eval' :: (Applicative m, Monad m) => LValue -> EvalT m LValue
eval' v@(Number _) = return v
eval' v@(String _) = return v
eval' v@(Bool _) = return v
eval' (List l) = evalForm l
eval' (Atom name) = atom name
eval' v@(Function _) = return v

evalForm :: (Applicative m, Monad m) => LList -> EvalT m LValue
evalForm [Atom "quote", e] = return e
evalForm (funExp : argExps) = do
  fun <- eval' funExp
  args <- mapM eval' argExps
  apply fun args
evalForm [] = throwError "can't eval empty list"


apply :: Monad m => LValue -> [LValue] -> EvalT m LValue
apply (Function f) args = return $ f args
apply v _ = throwError $ "not a function: " ++ show v


atom :: (Applicative m, Monad m) => LAtom -> EvalT m LValue
atom name = EvalT $ do
  env <- get
  (lookup name $ unEnv env) ?? ("not found: " ++ name)


initialEnv :: Env
initialEnv = Env $ map (second Function) [
  ]
