{-# LANGUAGE NoImplicitPrelude #-}

module Language.Pal.Repl (
    ReplError
  , rep
  ) where

import Prelude hiding (read)
import Control.Applicative (Applicative)
import Control.Error
import Control.Monad.Trans (lift)
import Text.Parsec (parse, ParseError)

import Language.Pal.Eval (Error, eval)
import Language.Pal.Parser (expr)
import Language.Pal.Types


data ReplError = ParseError ParseError | EvalError Error
  deriving (Show)

rep :: IO ()
rep = runEitherT rep' >>= handle where
  handle (Right ()) = return ()
  handle (Left e) = print e
  rep' = do
    input <- lift       getContents
    e <- hoistEither $  read "<stdin>" input
    v <- EitherT $      eval' e
    lift $              print v


read :: FilePath -> String -> Either ReplError LValue
read src = fmapL ParseError . parse expr src

eval' :: (Applicative m, Monad m) => LValue -> m (Either ReplError LValue)
eval' = fmap (fmapL EvalError . fst) . eval
