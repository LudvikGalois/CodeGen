{-# Language RankNTypes #-}

module CodeGen.BlockGen where

import CodeGen.Monad
import CodeGen.AST
import Control.Lens
import Control.Monad.State
import LLVM.General.AST.AddrSpace
import LLVM.General.AST (Module, Named ((:=)))
import qualified LLVM.General.AST as LLVM 
import qualified LLVM.General.AST.Instruction as INST
import qualified LLVM.General.AST.Constant as Const

type BlockGen a = StateT BlockGenInfo (State CodeGenInfo) a

type BlockGenInfo = LLVM.BasicBlock

runBlockGen c name term = execStateT c (LLVM.BasicBlock name [] term)

name :: forall f . Functor f => (LLVM.Name -> f LLVM.Name) -> LLVM.BasicBlock -> f LLVM.BasicBlock
name f (LLVM.BasicBlock n instr term) = fmap (\x -> LLVM.BasicBlock x instr term) $ f n
instructions :: forall f . Functor f => ([LLVM.Named LLVM.Instruction] -> f [LLVM.Named LLVM.Instruction]) 
    -> LLVM.BasicBlock -> f LLVM.BasicBlock
instructions f (LLVM.BasicBlock n instr term) = fmap (\x -> LLVM.BasicBlock n x term) $ f instr
terminator :: forall f . Functor f => (LLVM.Named LLVM.Terminator -> f (LLVM.Named LLVM.Terminator)) -> LLVM.BasicBlock -> f LLVM.BasicBlock
terminator f (LLVM.BasicBlock n instr term) = fmap (\x -> LLVM.BasicBlock n instr x) $ f term

newVar :: LLVM.Type -> BlockGen Var
newVar t = do
   n <- lift newName
   instructions %= (++ [n := INST.Alloca t Nothing 0 []])
   return (t,n)

append :: LLVM.Instruction -> BlockGen ()
append i = do
   instr <- lift newName
   instructions %= (++ [instr := i])

-- x := y 

assignIntConstant :: Var -> Integer -> BlockGen()
assignIntConstant (typeofx, x) y =
   append $ LLVM.Store False 
     (LLVM.LocalReference (LLVM.PointerType typeofx (AddrSpace 0)) x) 
     (LLVM.ConstantOperand (Const.Int (LLVM.typeBits typeofx) y)) Nothing 0 []

assign :: Var -> LLVM.Name -> BlockGen ()
assign (typeofx,x) y =
   append $ LLVM.Store False 
     (LLVM.LocalReference (LLVM.PointerType typeofx (AddrSpace 0)) x) 
     (LLVM.LocalReference typeofx y) Nothing 0 []

add :: Var -> Var -> BlockGen LLVM.Name
add (typeofx, x) (typeofy, y) = do
   x' <- lift newName
   y' <- lift newName
   z <- lift newName
   instructions %= (++
     [ (x' := INST.Load False (LLVM.LocalReference (LLVM.PointerType typeofx (AddrSpace 0)) x) Nothing 0 [])
     , (y' := INST.Load False (LLVM.LocalReference (LLVM.PointerType typeofy (AddrSpace 0)) y) Nothing 0 [])
     , (z  := INST.Add False False (LLVM.LocalReference typeofx x') (LLVM.LocalReference typeofy y') [] )
     ])
   return z

sub :: Var -> Var -> BlockGen LLVM.Name
sub (typeofx, x) (typeofy, y) = do
   x' <- lift newName
   y' <- lift newName
   z <- lift newName
   instructions %= (++
     [ (x' := INST.Load False (LLVM.LocalReference (LLVM.PointerType typeofx (AddrSpace 0)) x) Nothing 0 [])
     , (y' := INST.Load False (LLVM.LocalReference (LLVM.PointerType typeofy (AddrSpace 0)) y) Nothing 0 [])
     , (z  := INST.Sub False False (LLVM.LocalReference typeofx x') (LLVM.LocalReference typeofy y') [] )
     ])
   return z


