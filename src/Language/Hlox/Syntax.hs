{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Hlox.Syntax
  ( Value(..)
  , Expr(..)
  ) where

import qualified Data.Text as T
import Data.String (IsString)

newtype Ident = Ident { getIdent :: T.Text }
  deriving (Show, Eq, IsString)

data Value
  = Boolean Bool
  | Number Double
  | String T.Text
  | Nil
  deriving (Show, Eq)

data UnOp
  = Neg
  | Not
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Lt
  | LtEq
  | Gt
  | GtEq
  | Eq
  | NEq
  | And
  | Or
  deriving (Show, Eq)

data Expr
  = Lit Value
  | Var Ident
  | UnOpApp UnOp Expr
  | BinOpApp BinOp Expr Expr
  | Member Expr Ident
  deriving (Show, Eq)

data Stmt
  = VarDecl Ident (Maybe Expr)
  | Print Expr
  | ExprStmt Expr
  | IfElse Expr Stmt Stmt
  | While Expr Stmt
  | For Stmt Expr Stmt Stmt
  | FnCall Expr [Expr]
  | FnDef Ident [Ident] [Stmt]
  | Return Expr
  | Block [Stmt]
  deriving (Show, Eq)

data MethDef = MethDef Ident [Ident] [Stmt]
  deriving (Show, Eq)

data ClassDef = ClassDef Ident (Maybe Ident) [MethDef]
  deriving (Show, Eq)
