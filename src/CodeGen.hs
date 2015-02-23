{-# LANGUAGE RankNTypes #-}
module CodeGen where

import CodeGen.AST
import CodeGen.Monad
import CodeGen.BlockGen

import Control.Monad.State
import Control.Lens
import LLVM.General.AST (Module, Named ((:=)))
import qualified LLVM.General.AST as LLVM 
import qualified LLVM.General.AST.Global as LLVM
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as Const
import qualified LLVM.General.PassManager as Opt
import Data.Word
import qualified LLVM.General.Module as Mod
import LLVM.General.Context
import LLVM.General.Target
import Control.Monad.Except

testStuff = compile (runCodeGen test)

test :: CodeGen ()
test = do
  program.moduleName .= "Test"
  instr <- newName
  mainBlock <- runBlockGen buildMainBlock' (LLVM.Name "mainBlock") (instr := LLVM.Ret (Just (LLVM.ConstantOperand (Const.Int 32 0))) [])
  n <- newName
  newFunction "main" [] (LLVM.IntegerType 32) [mainBlock]
  extern "putchar" [LLVM.Parameter (LLVM.IntegerType 8) n []] (LLVM.IntegerType 32)

call fun args = LLVM.Call False CC.C [] fun args [] []

buildMainBlock' = do
  x <- newVar (LLVM.IntegerType 8)
  y <- newVar (LLVM.IntegerType 8)
  assignIntConstant x 20
  assignIntConstant y 14
  z <- add x y
  append $ call
      (Right $ LLVM.ConstantOperand (Const.GlobalReference (LLVM.FunctionType (LLVM.IntegerType 32) 
        [LLVM.IntegerType 8] False) (LLVM.Name "putchar")))
      [((LLVM.LocalReference (LLVM.IntegerType 8) z), [])]

buildMainBlock = do
  instr1 <- newName
  instr2 <- newName
  return $ buildBlock "mainBlock" 
    [instr1 := call
      (Right $ LLVM.ConstantOperand (Const.GlobalReference (LLVM.FunctionType (LLVM.IntegerType 32) 
        [LLVM.IntegerType 8] False) (LLVM.Name "putchar")))
      [(LLVM.ConstantOperand (Const.Int 8 33), [])]]
    (instr2 := LLVM.Ret (Just (LLVM.ConstantOperand (Const.Int 32 0))) [])

extern s args ret = newFunction s args ret []

newFunction :: String -> [LLVM.Parameter] -> LLVM.Type -> [LLVM.BasicBlock] -> CodeGen ()
newFunction s args ret b =  program.moduleDefinitions %= ((:) $ LLVM.GlobalDefinition $ LLVM.functionDefaults 
  { LLVM.name = LLVM.Name s
  , LLVM.parameters = (args, False)
  , LLVM.returnType = ret
  , LLVM.basicBlocks = b
  })

buildBlock :: String -> [LLVM.Named LLVM.Instruction] -> LLVM.Named LLVM.Terminator -> LLVM.BasicBlock
buildBlock s i t = LLVM.BasicBlock (LLVM.Name s) i t

printAssem mod = withContext $ \c -> runExceptT $ Mod.withModuleFromAST c mod (\m -> optimise m >> Mod.moduleLLVMAssembly m >>= putStrLn)

compile testModule = withContext $ \c -> runExceptT $ Mod.withModuleFromAST c testModule 
     (\m -> optimise m >> (runExceptT $ withDefaultTargetMachine $ \t -> runExceptT $ Mod.writeObjectToFile t (Mod.File "foo") m))

optimise :: Mod.Module -> IO Bool
optimise m = Opt.withPassManager
  (Opt.CuratedPassSetSpec (Just 3) Nothing Nothing (Just True) Nothing Nothing Nothing Nothing Nothing Nothing) (\p -> Opt.runPassManager p m)

