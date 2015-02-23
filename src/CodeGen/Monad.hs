{-# LANGUAGE RankNTypes #-}
module CodeGen.Monad where

import Control.Monad.State
import Control.Lens
import LLVM.General.AST (Module, Named ((:=)))
import qualified LLVM.General.AST as LLVM 
import Data.Word

data CodeGenInfo = CodeGenInfo { _program :: Module
                               , _ident :: Word
                               }

-- Template Haskell won't work because LLVM is a pain, so I'll define the lenses by hand
program :: forall f . Functor f => (Module -> f Module) -> CodeGenInfo -> f CodeGenInfo
program f c = fmap (\x -> c {_program = x}) $ f (_program c)

ident ::  forall f . Functor f => (Word -> f Word) -> CodeGenInfo -> f CodeGenInfo
ident f c = fmap (\x -> c {_ident = x}) $ f (_ident c)

moduleName :: forall f . Functor f => (String -> f String) -> Module -> f Module
moduleName f c = fmap (\x -> c {LLVM.moduleName = x}) $ f (LLVM.moduleName c)

moduleDefinitions :: forall f . Functor f => ([LLVM.Definition] -> f [LLVM.Definition]) -> Module -> f Module
moduleDefinitions f c = fmap (\x -> c {LLVM.moduleDefinitions = x}) $ f (LLVM.moduleDefinitions c)

type CodeGen a = State CodeGenInfo a

runCodeGen :: CodeGen a -> Module
runCodeGen c = _program $ execState c (CodeGenInfo LLVM.defaultModule 0)

newName :: CodeGen LLVM.Name
newName = do
   n <- use ident
   ident += 1
   return (LLVM.UnName n)

