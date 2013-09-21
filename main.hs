import Language.Pal.Parser

import Text.Parsec (parseTest)

main :: IO ()
main = do
  input <- getContents
  parseTest expr input
