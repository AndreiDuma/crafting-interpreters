-- TODO: Explicit exports
module Hlox.Syntax where

import Data.Text (Text)

----------- Program -----------

data Program d = Program [d]

---- Program type class and instances ----

class Prog p

instance Decl d => Prog (Program d)

----------- Declarations -----------

data VarDecl e = VarDecl Text (Maybe e)
data StmtDecl s = StmtDecl s

---- Decl type class and instances ----

class Decl d

instance Expr e => Decl (VarDecl e)
instance Stmt s => Decl (StmtDecl s)

----------- Statements -----------

data BlockStmt d = BlockStmt [d]
data IfStmt e s = IfStmt e s (Maybe s)
data WhileStmt e s = WhileStmt e s
data PrintStmt e = PrintStmt e
data ExprStmt e = ExprStmt e

---- Stmt type class and instances ----

class Stmt s

instance Decl d => Stmt (BlockStmt d)
instance (Expr e, Stmt s) => Stmt (IfStmt e s)
instance (Expr e, Stmt s) => Stmt (WhileStmt e s)
instance Expr e => Stmt (PrintStmt e)
instance Expr e => Stmt (ExprStmt e)

----------- Expressions -----------

-- Identifiers
data Identifier = Identifier Text

-- Literals
data LiteralNil = LiteralNil
data LiteralBoolean = LiteralBoolean Bool
data LiteralNumber = LiteralNumber Double
data LiteralString = LiteralString Text

-- Boolean operators
data Not e = Not e
data And l r = And l r
data Or l r = Or l r

-- Number operators
data Negate e = Negate e
data Plus l r = Plus l r
data Minus l r = Minus l r
data Star l r = Star l r
data Slash l r = Slash l r

-- Comparison operators
data Equal l r = Equal l r
data NotEqual l r = NotEqual l r
data Less l r = Less l r
data LessOrEqual l r = LessOrEqual l r
data Greater l r = Greater l r
data GreaterOrEqual l r = GreaterOrEqual l r

-- Assignment
data Assignment e = Assignment Text e

-- Parentheses
data Grouping e = Grouping e

---- Expr type class and instances ----
class Expr e

-- Identifiers
instance Expr Identifier

-- Literals
instance Expr LiteralNil
instance Expr LiteralBoolean
instance Expr LiteralNumber
instance Expr LiteralString

-- Boolean operators
instance Expr e => Expr (Not e)
instance (Expr l, Expr r) => Expr (And l r)
instance (Expr l, Expr r) => Expr (Or l r)

-- Number operators
instance Expr e => Expr (Negate e)
instance (Expr l, Expr r) => Expr (Plus l r)
instance (Expr l, Expr r) => Expr (Minus l r)
instance (Expr l, Expr r) => Expr (Star l r)
instance (Expr l, Expr r) => Expr (Slash l r)

-- Comparison operators
instance (Expr l, Expr r) => Expr (Equal l r)
instance (Expr l, Expr r) => Expr (NotEqual l r)
instance (Expr l, Expr r) => Expr (Less l r)
instance (Expr l, Expr r) => Expr (LessOrEqual l r)
instance (Expr l, Expr r) => Expr (Greater l r)
instance (Expr l, Expr r) => Expr (GreaterOrEqual l r)

-- Assignment
instance Expr e => Expr (Assignment e)

-- Parentheses
instance Expr e => Expr (Grouping e)
