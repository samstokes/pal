module Language.Pal.Types where


type LList = [LValue]

data LValue = Atom { lvAtom :: LAtom }
           | List { lvList :: LList }
           | Number { lvNumber :: LNumber }
           | String { lvString :: LString }
           | Bool { lvBool :: Bool }
           | Function { lvFunction :: LFunction }

instance Show LValue where
  show (Atom a) = a
  show (List l) = '(' : unwords (map show l) ++ ")"
  show (Number n) = show n
  show (String s) = show s
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Function (LFunction name _)) = "#<function " ++ name ++ ">"


type LAtom = String

type LNumber = Integer

type LString = String

type EvalError = String


newtype Env = Env { unEnv :: [(LAtom, LValue)] }
  deriving (Show)

lookupAtom :: LAtom -> Env -> Maybe LValue
lookupAtom a = lookup a . unEnv

setAtom :: LAtom -> LValue -> Env -> Env
setAtom k v = Env . ((k, v) :) . unEnv


type TFunction = [LValue] -> Either EvalError LValue

data LFunction = LFunction {
    fName :: LAtom
  , fFunction :: TFunction
  }
