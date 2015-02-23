module CodeGen.AST where

import qualified LLVM.General.AST as LLVM

data Type = Int8
          | Int16
          | Int32
          | Int64

type Var = (LLVM.Type, LLVM.Name)

toLLVM :: Type -> LLVM.Type
toLLVM Int8  = LLVM.IntegerType 8
toLLVM Int16 = LLVM.IntegerType 16
toLLVM Int32 = LLVM.IntegerType 32
toLLVM Int64 = LLVM.IntegerType 64

data BinOp = Add
           | Sub


