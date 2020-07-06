{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Codegen where


import           Control.Applicative
import           Control.Monad.State
import           Data.ByteString.Short
import           Data.Function
import           Data.List
import qualified Data.Map                        as Map
import           Data.String
import           Data.Word
import           LLVM.AST
import qualified LLVM.AST                        as AST
import           LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute              as A
import qualified LLVM.AST.CallingConvention      as CC
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import           LLVM.AST.Global
import qualified LLVM.AST.Linkage                as L
import           LLVM.AST.Type
import           LLVM.AST.Typed                  (typeOf)


-- Copy and undeerstand code from llvm-hs-kaleidoscope/src/Codegen.hs
