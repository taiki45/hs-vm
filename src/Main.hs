import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import VM

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
                           return $ read <$> ignoreComment (lines content)

ignoreComment :: [String] -> [String]
ignoreComment s = catMaybes $ skip <$> s
            where skip ('#':_)  = Nothing
                  skip str
                    | null str  = Nothing
                    | otherwise = Just str

showHelp :: IO ()
showHelp = do putStrLn "Usage:"
              putStrLn "    hs-vm run PATH"


