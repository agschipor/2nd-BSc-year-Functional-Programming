{
module Parser(startEvaluation) where
import Lexer
import qualified Data.Map as Map
}

%name evaluate 
%tokentype { Token }
%error { parseError }

%left '+' '-'
%left '*' '/'
%left '^'
%right sqrt sin cos tan log10 log2
%right NEG

%token 
  double                   { TokenDouble $$ }
  var                      { TokenVar $$ }
  sqrt                     { TokenSqrt }
  sin                      { TokenSin }
  cos                      { TokenCos }
  tan                      { TokenTan }
  log10                    { TokenLog10 }
  log2                     { TokenLog2 }
  ln                       { TokenLn }
  '+'                      { TokenPlus }
  '-'                      { TokenMinus }
  '^'                      { TokenPow }
  '*'                      { TokenMul }
  '/'                      { TokenDiv }
  '('                      { TokenOB }
  ')'                      { TokenCB }
  '='                      { TokenEq }

%%
Exp   :: { Exp }
Exp   : Exp1                    { Exp1 $1 }
      | var '=' Exp1            { Equal $1 $3 }

Exp1  :: { Exp1 }
Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  :: { Term }
Term  : Term '*' UnTerm         { Mul $1 $3 }
      | Term '/' UnTerm         { Div $1 $3 }
      | Term '^' UnTerm         { Pow $1 $3 }
      | UnTerm                  { UnTerm $1 }

UnTerm :: { UnTerm }
UnTerm : sqrt UnTerm            { Sqrt $2 }
       | sin UnTerm             { Sin $2 }
       | cos UnTerm             { Cos $2 }
       | tan UnTerm             { Tan $2 }
       | log10 UnTerm           { Log10 $2 }
       | log2 UnTerm            { Log2 $2 }
       | ln UnTerm              { Ln $2 }
       | Factor                 { Factor $1 }

Factor :: { Factor }
Factor : double                 { Double $1 }
       | var                    { Var $1 }
       | '(' Exp ')'            { Brack $2 }
       | '-' UnTerm %prec NEG   { UnMinus $2 }


{
data Exp = Exp1 Exp1
         | Equal String Exp1
         deriving Show

data Exp1 = Plus Exp1 Term
          | Minus Exp1 Term
          | Term Term
          deriving Show

data Term = Mul Term UnTerm
          | Div Term UnTerm
          | Pow Term UnTerm
          | UnTerm UnTerm
          deriving Show

data UnTerm = Sqrt UnTerm
            | Sin UnTerm
            | Cos UnTerm
            | Tan UnTerm
            | Log10 UnTerm
            | Log2 UnTerm
            | Ln UnTerm
            | Factor Factor
            deriving Show

data Factor = Double Double
            | Var String
            | Brack Exp
            | UnMinus UnTerm
            deriving Show

parseError :: [Token] -> a
parseError _ = error "invalid expression"    


eliminate (Just a) = a
eliminate (Nothing) = 0

evalExp variables (Exp1 e) = evalExp1 variables e
evalExp variables (Equal s e1) = 0

evalExp1 variables (Plus e1 t) = (evalExp1 variables e1) + (evalTerm variables t)
evalExp1 variables (Minus e1 t) = (evalExp1 variables e1) - (evalTerm variables t)
evalExp1 variables (Term t) = evalTerm variables t

evalTerm variables (Mul t ut) = (evalTerm variables t) * (evalUnTerm variables ut)
evalTerm variables (Div t ut) =  let value = evalUnTerm variables ut in
                                  if value == 0 
                                    then error "Error: division by zero"
                                    else (evalTerm variables t) / value
evalTerm variables (Pow t ut) = (evalTerm variables t) ** (evalUnTerm variables ut)
evalTerm variables (UnTerm ut) = evalUnTerm variables ut

evalUnTerm variables (Sqrt ut) = let value = evalUnTerm variables ut in
                          if value < 0
                            then error "Error: cannot calculate sqrt of a negative number"
                            else sqrt (value)
evalUnTerm variables (Sin ut) = sin (evalUnTerm variables ut)
evalUnTerm variables (Cos ut) = cos (evalUnTerm variables ut)
evalUnTerm variables (Tan ut) = tan (evalUnTerm variables ut)
evalUnTerm variables (Log10 ut) = let value = evalUnTerm variables ut in
                            if value <= 0
                              then error "Error: cannot calculate log10 of a negative number"
                              else logBase 10 (value)
evalUnTerm variables (Log2 ut) = let value = evalUnTerm variables ut in  
                          if value <= 0
                            then error "Error: cannot calculate log2 of a negative number"
                            else logBase 2 (value)
evalUnTerm variables (Ln ut) = let value = evalUnTerm variables ut in  
                          if value <= 0
                            then error "Error: cannot calculate ln of a negative number"
                            else log (value)
evalUnTerm variables (Factor f) = evalFactor variables f

evalFactor variables (Double d) = d
evalFactor variables (Var v) = let value = Map.lookup v variables in
                        if value == Nothing 
                          then error "Error: variable not defined" 
                          else eliminate value
evalFactor variables (Brack e) = evalExp variables e
evalFactor variables (UnMinus ut) = -1 * (evalUnTerm variables ut)


startEvaluation [] variables = return ()
startEvaluation (x:xs) variables = do 
                    let expression = evaluate (alexScanTokens x)

                    case expression of
                      (Equal s e1) -> startEvaluation (xs) (Map.insert s (evalExp1 variables e1) variables)
                      (Exp1 e) -> do 
                                    print (evalExp variables expression)
                                    startEvaluation (xs) variables

}