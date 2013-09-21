{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Pal.Eval
  ( eval
  , Error
  ) where

import Control.Applicative
import Control.Error
import Control.Monad.Error (MonadError)
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

evalForm :: Monad m => LList -> m LValue
evalForm [Atom "quote", e] = return e


initialEnv :: Env
initialEnv = Env [
  ]

atom :: (Applicative m, Monad m) => LAtom -> EvalT m LValue
atom name = EvalT $ do
  env <- get
  (lookup name $ unEnv env) ?? ("not found: " ++ name)
