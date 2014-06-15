{
module Lexer (Token(..),P,runP,lexer) where
import AST
import Control.Monad
import Data.Word
}

tokens :-
       $white+			;
       true			{TTrue}
       false			{TFalse}
       0			{TZero}
       succ			{TSucc}
       pred			{TPred}
       if			{TIf}
       then			{TThen}
       else			{TElse}
       iszero			{TIsZero}

{
data Token = 
     TTrue
     | TFalse
     | TZero
     | TSucc
     | TPred
     | TIf
     | TThen
     | TElse
     | TIsZero
     | TEOF
     deriving (Eq,Show)

-- The functions that must be provided to Alex's basic interface
type AlexInput = [Word8]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (b:bs) = Just (b,bs)
alexGetByte []    = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

-- The underlying result type of our parser
type ParseResult a = Maybe (a,AlexInput)

-- Our Parser monad
newtype P a = P {unP::AlexInput -> ParseResult a}

runP::P a -> AlexInput -> ParseResult a
runP (P m) s = m s

instance Monad P where
	 return = returnP
	 (>>=) = thenP
	 fail = failP

returnP::a -> P a
returnP a = P $ \s -> Just (a,s)

thenP::P a -> (a -> P b) -> P b
(P m) `thenP` k  = P $ \s ->
      case m s of
      	   Just (v,s1) -> (unP (k v)) s1
	   Nothing -> Nothing

failP::String -> P a
failP _ = P $ \s -> Nothing

-- Action to read a token
readToken::P Token
readToken = P $ \s ->
      case alexScan s 0 of
      	   AlexEOF -> Just (TEOF,s)
	   AlexError _ -> Nothing
	   AlexSkip inp' _ -> (unP readToken) inp'
	   AlexToken inp' _ tk -> Just (tk,inp')

-- The lexer function to be passed to Happy
lexer::(Token -> P a)->P a
lexer cont = readToken >>= cont

}    
