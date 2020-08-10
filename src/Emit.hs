{-# LANGUAGE OverloadedStrings #-}


module Emit where


import           LLVM.Context
import           LLVM.Module

import qualified LLVM.AST                        as AST
import qualified LLVM.AST.AddrSpace              as A
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Float                  as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.ByteString.Char8           as B
import           Data.ByteString.Short
import           Data.Int
import qualified Data.Map                        as Map
import           Data.Word

import           Codegen
import qualified Syntax                          as S


one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one


toSig :: [S.Expr] -> [(AST.Type, AST.Name)]
toSig = map (\(S.Var x) -> (double, AST.Name x))


codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
    define double name fnargs (\typ -> bls)
    where
        fnargs = toSig args
        bls = do
            forM args $ \(S.Var a) -> do
                var <- alloca double
                store var (local double (AST.Name a))
                assign a var
            cgen body >>= ret
codegenTop (S.Extern name args) = do
    external double name fnargs
    where fnargs = toSig args
codegenTop exp = do
    define double "main" [] (\typ -> cgen exp)


-- Operations

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test


binops = Map.fromList [
      (S.Plus, fadd)
    , (S.Minus, fsub)
    , (S.Times, fmul)
    , (S.Divide, fdiv)
    , (S.Less, lt)
  ]


cgen :: S.Expr -> Codegen AST.Operand
cgen (S.BinOp S.Equal (S.Var var) val) = do
    a <- getvar var
    cval <- cgen val
    store a cval
    return cval
cgen (S.BinOp op a b) = do
    case Map.lookup op binops of
        Just f  -> do
            ca <- cgen a
            cb <- cgen b
            f ca cb
        Nothing -> error "No such operator"
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
    largs <- mapM cgen args
    call (externf fnty (AST.Name fn)) largs
    where
        fnargs = map (const double) args
        fnty = fnType fnargs
cgen (S.If cond tr fl) = do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    cond <- cgen cond
    test <- fcmp FP.ONE false cond
    cbr test ifthen ifelse

    setBlock ifthen
    trval <- cgen tr
    br ifexit
    ifthen <- getBlock

    setBlock ifelse
    flval <- cgen fl
    br ifexit
    ifelse <- getBlock

    setBlock ifexit
    phi double [(trval, ifthen), (flval, ifelse)]
cgen (S.For ivar start cond step body) = do
    forloop <- addBlock "for.loop"
    forexit <- addBlock "for.exit"

    i <- alloca double
    istart <- cgen start
    stepval <- cgen step

    store i istart
    assign ivar i
    br forloop

    setBlock forloop
    cgen body
    ival <- load i
    inext <- fadd ival stepval
    store i inext

    cond <- cgen cond
    test <- fcmp FP.ONE false cond
    cbr test forloop forexit

    setBlock forexit
    return zero


-- Compilation

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return


codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = return newast
    where
        modn    = mapM codegenTop fns
        newast  = runLLVM mod modn
