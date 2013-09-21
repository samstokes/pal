{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Pal.Eval
  ( eval
  ) where

import Control.Monad.State

import Language.Pal.Types


newtype Env = Env { unEnv :: [(LAtom, LValue)] }

newtype EvalT m a = EvalT { unEvalT :: StateT Env m a } deriving (Monad, MonadState Env)

runEvalT :: Monad m => EvalT m a -> Env -> m (a, Env)
runEvalT = runStateT . unEvalT


eval :: Monad m => LValue -> m (LValue, Env)
eval val = runEvalT (eval' val) initialEnv

eval' :: Monad m => LValue -> EvalT m LValue
eval' v@(Number _) = return v
eval' v@(String _) = return v
eval' v@(Bool _) = return v
eval' (List l) = evalForm l

evalForm :: Monad m => LList -> m LValue
evalForm [Atom "quote", e] = return e


initialEnv :: Env
initialEnv = Env [
  ]
