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


newtype ReplT m a = ReplT { unReplT :: EitherT ReplError m a }
  deriving (Monad, Applicative, Functor)

instance MonadTrans ReplT where
  lift = ReplT . lift

runReplT :: Monad m => ReplT m a -> m (Either ReplError a)
runReplT = runEitherT . unReplT

liftEither :: Monad m => Either ReplError a -> ReplT m a
liftEither = ReplT . hoistEither


rep :: IO ()
rep = runReplT (rep' initialEnv) >>= handle where
  handle :: Either ReplError () -> IO ()
  handle (Right ()) = return ()
  handle (Left e) = print e
  rep' :: Env -> ReplT IO ()
  rep' env = do
    input <- lift       getContents
    e <- liftEither $   read "<stdin>" input
    v <-                eval' e env
    lift $              print v


read :: FilePath -> String -> Either ReplError LValue
read src = fmapL ParseError . parse expr src

eval' :: (Applicative m, Monad m) => LValue -> Env -> ReplT m LValue
eval' val env = do
  (eOrV, _) <- eval val env
  liftEither $ either (throwE . EvalError) Right eOrV
