import Language.Pal.Eval
import Language.Pal.Parser

import Text.Parsec (parse)

main :: IO ()
main = do
  input <- getContents
  case parse expr "<stdin>" input of
    Right l -> print $ eval l
    Left err -> putStrLn $ "parse error: " ++ show err
