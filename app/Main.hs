{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Codegen                  (emptyModule)
import           Emit                     (codegen)
import           JIT                      (runJIT)
import           Parser                   (parseTopLevel)

import           Control.Monad.Trans

import           System.Console.Haskeline
import           System.Environment
import           System.IO

import qualified LLVM.AST                 as AST


initModule :: AST.Module
initModule = emptyModule "my cool jit"


process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseTopLevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      rc <- runJIT ast
      return $ Just ast


processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule


repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "Ks> "
    case minput of
      Nothing -> outputStrLn "Exiting..."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing   -> loop mod


main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
