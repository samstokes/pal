module Language.Pal.Types where


type LList = [LValue]

data LValue = Atom LAtom
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
  show (Function _) = "#<function>"


type LAtom = String

type LNumber = Integer

type LString = String

type LFunction = [LValue] -> LValue
