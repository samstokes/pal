{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Pal.Types where

import Data.Monoid


type LList = [LValue]

nil :: LValue
nil = List []

data LValue = Atom { lvAtom :: LAtom }
           | List { lvList :: LList }
           | Number { lvNumber :: LNumber }
           | String { lvString :: LString }
           | Bool { lvBool :: Bool }
           | BuiltinFunction { lvBIFunction :: LBuiltinFunction }
           | LispFunction { lvLFunction :: LLispFunction }

instance Show LValue where
  show (Atom a) = a
  show (List l) = '(' : unwords (map show l) ++ ")"
  show (Number n) = show n
  show (String s) = show s
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (BuiltinFunction (LBuiltinFunction name _)) = "#<function " ++ name ++ ">"
  show (LispFunction (LLispFunction _ params _)) = "#<function lambda (" ++ unwords params ++ ")>"


type LAtom = String

type LNumber = Integer

type LString = String

type EvalError = String


newtype Env = Env { unEnv :: [(LAtom, LValue)] }
  deriving (Show, Monoid)

lookupAtom :: LAtom -> Env -> Maybe LValue
lookupAtom a = lookup a . unEnv

setAtom :: LAtom -> LValue -> Env -> Env
setAtom k v = Env . ((k, v) :) . unEnv


type TFunction = [LValue] -> Either EvalError LValue

data LBuiltinFunction = LBuiltinFunction {
    bfName :: LAtom
  , bfFunction :: TFunction
  }

data LLispFunction = LLispFunction {
    lfScope :: Env
  , lfParams :: [LAtom]
  , lfBody :: [LValue]
  }
