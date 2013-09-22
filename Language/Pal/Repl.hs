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

import Language.Pal.Eval (eval)
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
rep = runReplT rep' >>= handle where
  handle :: Either ReplError () -> IO ()
  handle (Right ()) = return ()
  handle (Left e) = print e
  rep' :: ReplT IO ()
  rep' = do
    input <- lift       getContents
    e <- liftEither $   read "<stdin>" input
    v <-                eval' e
    lift $              print v


read :: FilePath -> String -> Either ReplError LValue
read src = fmapL ParseError . parse expr src

eval' :: (Applicative m, Monad m) => LValue -> ReplT m LValue
eval' val = do
  (eOrV, _) <- eval val
  liftEither $ either (throwE . EvalError) Right eOrV
