import System.Environment (getArgs)

import VM
import Parser

main :: IO ()
main = do args <- getArgs
          case args of
              ("run":path:_) -> readFile path >>= run path
              ("run":_)      -> getContents >>= run "STDIN"
              _              -> showHelp

run :: FilePath -> String -> IO ()
run path content = do instructions <- readInstructions path content
                      putStrLn .show . takeResult $ runVM instructions

readInstructions :: String -> FilePath -> IO [Instruction]
readInstructions path str = case parse instructionsParser path str of
                                Left e  -> error $ show e
                                Right v -> return v

showHelp :: IO ()
showHelp = do putStrLn "Usage:"
              putStrLn "    hs-vm run PATH"
