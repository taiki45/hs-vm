import System.Environment (getArgs)

import VM
import Parser

main :: IO ()
main = do args <- getArgs
          case args of
              ("run":path:_) -> run path
              _              -> showHelp

run :: FilePath -> IO ()
run path = do instructions <- readInstructions path
              putStrLn .show . takeResult $ runVM instructions

readInstructions :: FilePath -> IO [Instruction]
readInstructions path = do content <- readFile path
                           case parse instructionsParser path content of
                               Left e  -> error $ show e
                               Right v -> return v

showHelp :: IO ()
showHelp = do putStrLn "Usage:"
              putStrLn "    hs-vm run PATH"


