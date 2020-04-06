module Language.Hlox.Types
  ( Type(..)
  ) where

data Type
  = BooleanType
  | NumberType
  | StringType
  | NilType
  deriving (Show, Eq)
