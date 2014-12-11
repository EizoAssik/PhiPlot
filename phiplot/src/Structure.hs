module Structure ( Token(..),
                   Atom(..),
                   Expr(..),
                   Stmt(..),
                   (~>>),
                   ParseResult(..),
                   ParseFlow(..),
                   PhiAST(..) ) where

import Data.Char hiding ( format )

-- ERROR 用于在语法分析阶段标记错误
data Token = Name     String
           | Real     Float
           | LP  | RP   | LB   | RB   | COMMA | SEMICOLON
           | FOR | FROM | TO   | STEP | IS    | ERROR | EOF
           | ADD | SUB  | MUL  | DIV  | POWER | Unknown String
           | IF  | ELSE | ELIF | THEN
           | AND | OR   | NOT
           | EQ  | NE   | LE   | GE   | GT    | LT
           | DEF | RETURN
           deriving (Show, Eq)

data Atom = Not      Expr
          | Subexpr  Expr
          | Negative Expr
          | Imm      { value :: Float }
          | Ref      { name  :: String }
          | Funcall  Atom Expr
          | Void
          deriving (Show, Eq)

data Expr = Elem    { elem :: Atom }
          | PhiList Expr  Expr
          | ArgList Expr  Expr
          | BinOp   Token Expr Expr
          | Logic   Token Expr Expr
          | Skip
          deriving (Show, Eq)

data Stmt = Direct   Atom
          | ExprEval Expr
          | Return   Expr
          | Set      Atom Expr
          | Block    Stmt Stmt
          | Def      Atom Expr Stmt
          | If       Expr Stmt Stmt
          | For      Atom Expr Expr Expr Stmt
          | NOP
          | END 
          deriving (Show, Eq)

data ParseResult a = Success a [Token]
                   | Error   Token [Token] String
                   deriving (Show, Eq)

class ParseFlow f where
-- 连续匹配
    (|->)   :: f a -> (a -> [Token] -> f b) -> f b
-- 无视错误连续匹配
    (|->>)  :: f a -> ([Token] -> f b) -> f b
-- 检查关键字并继续匹配
    (|=?>)  :: f a -> Token -> f a
-- 尝试检查关键字，失配则略过
    (|=?>>) :: f a -> Token -> f a
-- 尝试匹配组合
    (|+?>)  :: f a -> (f a -> f b) -> f b
-- 通用类型转换
    wrap    :: f a -> String -> f b

instance ParseFlow ParseResult where
    m |->  g = case m of
                   Success p rs -> g p rs
                   Error x xs y -> Error x xs y

    m |->> g = case m of
                   Success p rs -> g rs
                   Error x xs y -> g xs

    m |=?> t = case m of
                   succ@(Success p (x:xs)) ->
                       if x == t
                           then Success p xs
                           else Error x xs ("Cannot match token: " ++ (show t))
                   err@(Error _ _ _) -> wrap err $ "When using |=?> " ++ show t

    m |=?>> t = case m of
                    succ@(Success p xs@(x:s)) ->
                        if x == t
                            then Success p s
                            else Success p xs
                    err@(Error _ _ _) -> wrap err $ "When using |=?>> " ++ show t

    m |+?> g = case m of
                   succ@(Success p (x:xs)) -> g m
                   err@(Error x s i) -> wrap err "When using |+?>"

    wrap m a = case m of
                 Error x s i  -> Error x s (i ++ "\n\t\t>> " ++ a)

class ParseTree t where
    (~>>) :: (t -> a) -> t -> [a]

instance ParseTree Expr where
    f ~>> expr = 
        case expr of
            PhiList l r -> (f ~>> l) ++ (f ~>> r) 
            ArgList l r -> (f ~>> l) ++ (f ~>> r)
            expr        -> [f expr]

instance ParseTree Stmt where
    f ~>> stmt = 
        case stmt of
            Block l r   -> (f ~>> l) ++ (f ~>> r)
            stmt        -> [f stmt]

data PhiAST = COND   PhiAST PhiAST PhiAST
            | LOOP   String PhiAST PhiAST PhiAST PhiAST
            | BLOCK             [PhiAST]
            | SET    String      PhiAST
            | CALL   String     [PhiAST]
            | DEFINE String [String] PhiAST
            | SOP    Token  PhiAST
            | OP     Token  PhiAST  PhiAST
            | REF    String
            | NUM    Float
            | PASS
            deriving (Show, Eq)


