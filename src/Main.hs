import System.Environment (getArgs)

import VM
import Parser

main :: IO ()
main = do args <- getArgs
          case args of
              ("help":_)   -> showHelp
              ("-h":_)     -> showHelp
              ("--help":_) -> showHelp
              (path:_)     -> readFile path >>= run path
              _            -> if null args
                                  then getContents >>= run "STDIN"
                                  else showHelp

run :: FilePath -> String -> IO ()
run path content = do instructions <- readInstructions path content
                      print . takeResult $ runVM instructions

readInstructions :: FilePath -> String -> IO [Instruction]
readInstructions path str = case parse instructionsParser path str of
                                Left e  -> error $ show e
                                Right v -> return v

showHelp :: IO ()
showHelp = do putStrLn "Usage:"
              putStrLn "    hs-vm PATH\n"
              putStrLn "From STDIN:"
              putStrLn "    hs-vm"
