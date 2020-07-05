module Main where


import           Control.Monad.Trans
import           Parser
import           System.Console.Haskeline


process :: String -> IO ()
process line = do
        let res = parseTopLevel line
        case res of
            Left err -> print err
            Right ex -> mapM_ print ex


main :: IO ()
main = runInputT defaultSettings loop
    where
        loop = do
            minput <- getInputLine "Ks> "
            case minput of
                Nothing    -> outputStrLn "Exiting..."
                Just input -> (liftIO $ process input) >> loop
