module Main (main) where
import System.IO
import System.IO.Error
import qualified Lexer as L
import qualified Parser as P
import qualified AST as S
import qualified Evaluator as E
import Codec.Binary.UTF8.String (encode)

output::S.Term -> String
output t = (show t)++"\n"++(show (E.eval t))

myREPL::IO()
myREPL = do
  putStr "> "
  l <- tryIOError getLine
  case l of
    Left e ->
      if isEOFError e
         then putStrLn "Bye!"
         else ioError e     
    Right s ->
      if s/="" 
      then 
        do 
          case L.runP P.parse (encode s) of
            Just (t,_) -> putStrLn (output t)
            Nothing -> putStrLn "!Parse Error"
          myREPL
      else
        myREPL

main::IO ()
main = do
  hSetBuffering stdout NoBuffering
  myREPL
