{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Pal.Repl (
    ReplError
  , rep
  ) where

import Prelude hiding (read)
import Control.Applicative (Applicative)
import Control.Error
import Control.Monad.State
import Text.Parsec (parse, ParseError)

import Language.Pal.Eval (eval, Env, initialEnv)
import Language.Pal.Parser (expr)
import Language.Pal.Types


data ReplError = ParseError ParseError | EvalError EvalError
  deriving (Show)


newtype ReplT m a = ReplT { unReplT :: EitherT ReplError (StateT Env m) a }
  deriving (Monad, Applicative, Functor, MonadState Env)

instance MonadTrans ReplT where
  lift = ReplT . lift . lift

runReplT :: Monad m => ReplT m a -> Env -> m (Either ReplError a, Env)
runReplT = runStateT . runEitherT . unReplT

liftEither :: Monad m => Either ReplError a -> ReplT m a
liftEither = ReplT . hoistEither


rep :: IO ()
rep = runReplT (rep' =<< lift getContents) initialEnv >>= void . handle where
  handle :: (Either ReplError LValue, Env) -> IO Env
  handle (eOrV, env') = either print print eOrV >> return env'
  rep' :: String -> ReplT IO LValue
  rep' input = do
    e <- liftEither $ read "<stdin>" input
    theEnv <- get
    eval' e theEnv


read :: FilePath -> String -> Either ReplError LValue
read src = fmapL ParseError . parse expr src

eval' :: (Applicative m, Monad m) => LValue -> Env -> ReplT m LValue
eval' val env = do
  (eOrV, env') <- eval val env
  put env'
  liftEither $ either (throwE . EvalError) Right eOrV
