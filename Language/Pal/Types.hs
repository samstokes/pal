module Language.Pal.Types
  ( LValue(..)
  , LList
  , LAtom
  , LNumber
  , LString
  ) where


type LList = [LValue]

data LValue = Atom LAtom
           | List LList
           | Number LNumber
           | String LString
           | Bool Bool

instance Show LValue where
  show (Atom a) = a
  show (List l) = '(' : unwords (map show l) ++ ")"
  show (Number n) = show n
  show (String s) = show s
  show (Bool True) = "#t"
  show (Bool False) = "#f"


type LAtom = String

type LNumber = Integer

type LString = String


