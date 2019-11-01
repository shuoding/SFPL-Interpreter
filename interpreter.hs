import Data.Char
import Data.List


data Token = NIL | INT Int | FUNC | PAIR
           | VAR String
           | ISNIL | ISINT | ISCLOSURE | ISPAIR
           | ADD | SUB | MUL | DIV
           | EQT | NEQ | LTH | LEQ | GTH | GEQ
           | LET | IF | CALL
           | FIRST | SECOND | GETINTLINE | PUTINTLINE
           | LPA | RPA
           deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize "" = []
tokenize (c : tl)
    | isSpace c = tokenize tl
    | isAlpha c = processAlpha (c : takeWhile isAlpha tl) : tokenize (dropWhile isAlpha tl)
    | isDigit c = processDigit (c : takeWhile isDigit tl) : tokenize (dropWhile isDigit tl)
    | c == '+' = ADD : tokenize tl
    | c == '-' = SUB : tokenize tl
    | c == '*' = MUL : tokenize tl
    | c == '/' = DIV : tokenize tl
    | c == '=' = if peekEqual
                 then EQT : tokenize (tail tl)
                 else error "lexical error: expecting =="
    | c == '!' = if peekEqual
                 then NEQ : tokenize (tail tl)
                 else error "lexical error: expecting !="
    | c == '<' = if peekEqual
                 then LEQ : tokenize (tail tl)
                 else LTH : tokenize tl
    | c == '>' = if peekEqual
                 then GEQ : tokenize (tail tl)
                 else GTH : tokenize tl
    | c == '(' = LPA : tokenize tl
    | c == ')' = RPA : tokenize tl
    | otherwise = error ("lexical error: unknown character " ++ [c])
    where peekEqual = (not.null $ tl) && (head tl == '=')
          processAlpha s
              | s == "nil" = NIL
              | s == "function" = FUNC
              | s == "pair" = PAIR
              | s == "isNil" = ISNIL
              | s == "isInt" = ISINT
              | s == "isClosure" = ISCLOSURE
              | s == "isPair" = ISPAIR
              | s == "let" = LET
              | s == "if" = IF
              | s == "call" = CALL
              | s == "first" = FIRST
              | s == "second" = SECOND
              | s == "getIntLine" = GETINTLINE
              | s == "putIntLine" = PUTINTLINE
              | otherwise = VAR s
          processDigit s = INT (read s)


data P = PNil | PInt Int | PFunc String String P | PPair P P
       | PVar String
       | PIsNil P | PIsInt P | PIsClosure P | PIsPair P
       | PAdd P P | PSub P P | PMul P P | PDiv P P
       | PEqt P P | PNeq P P | PLth P P | PLeq P P | PGth P P | PGeq P P
       | PLet String P P | PIf P P P | PCall P P
       | PFirst P | PSecond P | PGetIntLine | PPutIntLine P
       deriving (Show, Eq)


parseP :: [Token] -> (P, [Token])
parseP (NIL : tl) = (PNil, tl)
parseP ((INT i) : tl) = (PInt i, tl)
parseP ((VAR v) : tl) = (PVar v, tl)
parseP (GETINTLINE : tl) = (PGetIntLine, tl)
parseP (LPA : tl) = parseTail tl
parseP (t : tl) = error ("syntax error: invalid expression head token " ++ show t)
parseP [] = error "syntax error: empty expression"


checkRPA :: [Token] -> [Token]
checkRPA (RPA : tl) = tl
checkRPA _ = error "syntax error: missing right parenthesis"


parseTail :: [Token] -> (P, [Token])
parseTail (FUNC : (VAR f) : (VAR x) : tl0) = let (p, tl1) = parseP tl0
                                             in (PFunc f x p, checkRPA tl1)
parseTail (PAIR : tl0) = let (p1, tl1) = parseP tl0
                         in let (p2, tl2) = parseP tl1
                            in (PPair p1 p2, checkRPA tl2)
parseTail (ISNIL : tl0) = let (p1, tl1) = parseP tl0
                          in (PIsNil p1, checkRPA tl1)
parseTail (ISINT : tl0) = let (p1, tl1) = parseP tl0
                          in (PIsInt p1, checkRPA tl1)
parseTail (ISCLOSURE : tl0) = let (p1, tl1) = parseP tl0
                              in (PIsClosure p1, checkRPA tl1)
parseTail (ISPAIR : tl0) = let (p1, tl1) = parseP tl0
                           in (PIsPair p1, checkRPA tl1)
parseTail (ADD : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PAdd p1 p2, checkRPA tl2)
parseTail (SUB : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PSub p1 p2, checkRPA tl2)
parseTail (MUL : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PMul p1 p2, checkRPA tl2)
parseTail (DIV : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PDiv p1 p2, checkRPA tl2)
parseTail (EQT : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PEqt p1 p2, checkRPA tl2)
parseTail (NEQ : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PNeq p1 p2, checkRPA tl2)
parseTail (LTH : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PLth p1 p2, checkRPA tl2)
parseTail (LEQ : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PLeq p1 p2, checkRPA tl2)
parseTail (GTH : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PGth p1 p2, checkRPA tl2)
parseTail (GEQ : tl0) = let (p1, tl1) = parseP tl0
                        in let (p2, tl2) = parseP tl1
                           in (PGeq p1 p2, checkRPA tl2)
parseTail (LET : (VAR x) : tl0) = let (p1, tl1) = parseP tl0
                                  in let (p2, tl2) = parseP tl1
                                     in (PLet x p1 p2, checkRPA tl2)
parseTail (IF : tl0) = let (p1, tl1) = parseP tl0
                       in let (p2, tl2) = parseP tl1
                          in let (p3, tl3) = parseP tl2
                             in (PIf p1 p2 p3, checkRPA tl3)
parseTail (CALL : tl0) = let (p1, tl1) = parseP tl0
                         in let (p2, tl2) = parseP tl1
                            in (PCall p1 p2, checkRPA tl2)
parseTail (FIRST : tl0) = let (p1, tl1) = parseP tl0
                          in (PFirst p1, checkRPA tl1)
parseTail (SECOND : tl0) = let (p1, tl1) = parseP tl0
                           in (PSecond p1, checkRPA tl1)
parseTail (PUTINTLINE : tl0) = let (p1, tl1) = parseP tl0
                               in (PPutIntLine p1, checkRPA tl1)
parseTail (t : tl) = error ("syntax error: unknown expression body " ++ show t)
parseTail [] = error "syntax error: empty expression after a left parenthesis"


parse :: [Token] -> P
parse tokens = let (p, remaining) = parseP tokens
               in if null remaining
                  then p
                  else error "syntax error: redundant trailing tokens"


data Val = NilVal | IntVal Int | ClosureVal [(String, Val)] String String P | PairVal Val Val deriving (Eq)


instance Show Val where
    show NilVal = "nil"
    show (IntVal i) = show i
    show (ClosureVal env f x p) = show env ++ " " ++ f ++ " " ++ x ++ " {" ++ show p ++ "}"
    show (PairVal v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"


checkIntVal :: Val -> Int
checkIntVal (IntVal i) = i
checkIntVal _ = error "type error: expecting an Int value"


checkClosureVal :: Val -> ([(String, Val)], String, String, P)
checkClosureVal (ClosureVal savedEnv f x p) = (savedEnv, f, x, p)
checkClosureVal _ = error "type error: expecting a Closure value"


checkPairVal :: Val -> (Val, Val)
checkPairVal (PairVal v1 v2) = (v1, v2)
checkPairVal _ = error "type error: expecting a Pair value"


searchEnv :: [(String, Val)] -> String -> Val
searchEnv [] name = error ("semantic error: undefined variable" ++ name)
searchEnv ((name0, val0) : tl) name = if name == name0
                                      then val0
                                      else searchEnv tl name


interp :: [(String, Val)] -> P -> IO Val
interp env PNil = return NilVal
interp env (PInt i) = return (IntVal i)
interp env (PFunc f x p) = return (ClosureVal env f x p)
interp env (PPair p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return (PairVal val1 val2)
interp env (PVar name) = return (searchEnv env name)
interp env (PIsNil p) = do
    val <- interp env p
    return (isNil val)
    where isNil NilVal = IntVal 1
          isNil _ = IntVal 0
interp env (PIsInt p) = do
    val <- interp env p
    return (isInt val)
    where isInt (IntVal _) = IntVal 1
          isInt _ = IntVal 0
interp env (PIsClosure p) = do
    val <- interp env p
    return (isClosure val)
    where isClosure (ClosureVal _ _ _ _) = IntVal 1
          isClosure _ = IntVal 0
interp env (PIsPair p) = do
    val <- interp env p
    return (isPair val)
    where isPair (PairVal _ _) = IntVal 1
          isPair _ = IntVal 0
interp env (PAdd p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (checkIntVal val1 + checkIntVal val2)
interp env (PSub p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (checkIntVal val1 - checkIntVal val2)
interp env (PMul p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (checkIntVal val1 * checkIntVal val2)
interp env (PDiv p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (checkIntVal val1 `div` checkIntVal val2)
interp env (PEqt p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (if checkIntVal val1 == checkIntVal val2 then 1 else 0)
interp env (PNeq p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (if checkIntVal val1 /= checkIntVal val2 then 1 else 0)
interp env (PLth p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (if checkIntVal val1 < checkIntVal val2 then 1 else 0)
interp env (PLeq p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (if checkIntVal val1 <= checkIntVal val2 then 1 else 0)
interp env (PGth p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (if checkIntVal val1 > checkIntVal val2 then 1 else 0)
interp env (PGeq p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    return $ IntVal (if checkIntVal val1 >= checkIntVal val2 then 1 else 0)
interp env (PLet name p1 p2) = do
    val <- interp env p1
    interp ((name, val) : env) p2
interp env (PIf p1 p2 p3) = do
    val1 <- interp env p1
    if checkIntVal val1 /= 0 then interp env p2 else interp env p3
interp env (PCall p1 p2) = do
    val1 <- interp env p1
    val2 <- interp env p2
    let (savedEnv, f, x, p) = checkClosureVal val1 in interp ((x, val2) : (f, ClosureVal savedEnv f x p) : savedEnv) p
interp env (PFirst p) = do
    val <- interp env p
    let (val1, val2) = checkPairVal val in return val1
interp env (PSecond p) = do
    val <- interp env p
    let (val1, val2) = checkPairVal val in return val2
interp env PGetIntLine = getLine >>= return.(\i -> IntVal i).read.strip
    where strip = dropWhile isSpace.reverse.dropWhile isSpace.reverse
interp env (PPutIntLine p) = do
    val <- interp env p
    print.checkIntVal $ val
    return NilVal


interpret :: String -> IO Val
interpret = interp [].parse.tokenize