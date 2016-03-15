{
module Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9       
$alpha = [a-zA-Z]  
@doubleNumber = $digit+(\.$digit+)?
@var = (([_]($digit|$alpha|"_")+)|($alpha($alpha|$digit|"_")*))
@sqrt = ($white*)"sqrt"($white*)
@sin = ($white*)"sin"($white*)
@cos = ($white*)"cos"($white*)
@tan = ($white*)"tan"($white*)
@log10 = ($white*)"log10"($white*)
@log2 = ($white*)"log2"($white*)
@ln = ($white*)"ln"($white*)


tokens :-

  $white+                  ;
  @sqrt                    { \s -> TokenSqrt }
  @sin                     { \s -> TokenSin }
  @cos                     { \s -> TokenCos }
  @tan                     { \s -> TokenTan }
  @log10                   { \s -> TokenLog10 }
  @log2                    { \s -> TokenLog2 }
  @ln                      { \s -> TokenLn }
  @doubleNumber            { \s -> TokenDouble (read s) }
  @var                     { \s -> TokenVar s }
  "+"                      { \s -> TokenPlus }
  "-"                      { \s -> TokenMinus }
  "*"                      { \s -> TokenMul }
  "/"                      { \s -> TokenDiv }
  "("                      { \s -> TokenOB }
  ")"                      { \s -> TokenCB }
  "="                      { \s -> TokenEq }
  "^"                      { \s -> TokenPow }


{
data Token
      = TokenDouble Double
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenMul
      | TokenDiv
      | TokenOB
      | TokenCB
      | TokenSqrt
      | TokenLog10
      | TokenLog2
      | TokenLn
      | TokenSin
      | TokenCos
      | TokenTan
      | TokenPow
 deriving Show
}