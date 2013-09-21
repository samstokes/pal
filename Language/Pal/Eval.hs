module Language.Pal.Eval
  ( eval
  ) where

import Language.Pal.Types

eval :: LValue -> LValue
eval v@(Number _) = v
eval v@(String _) = v
eval v@(Bool _) = v
