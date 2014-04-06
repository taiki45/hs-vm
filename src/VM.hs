module VM
    ( Instruction (..)
    , runVM
    , takeResult
    , takePC
    , testRunVM ) where

import Control.Applicative ((<$>))
import Data.Array
import Data.Monoid
import Data.Foldable (foldMap)

import VM.Machine
import VM.Instruction

runVM :: [Instruction] -> Machine
runVM is = run (toArray $ instMorph <$> is) (setMain . setCounter 0 . prepare is $ initMachine)

setMain :: Machine -> Machine
setMain m = setCounter main m
        where main = lookupL "main" $ takeL m

run :: Array PC (Machine -> Machine) -> Machine -> Machine
run is m | end pc = m
         | otherwise = run is (is ! pc $ m)
        where pc = fromInteger . takePC $ m
              end = ((snd . bounds) is ==)

prepare :: [Instruction] -> Machine -> Machine
prepare is = appEndo . getDual $ foldMap (Dual . Endo . setLabel) is

toArray :: [a] -> Array PC a
toArray ls = listArray (0,toInteger $ length ls) ls

testRunVM :: [Instruction] -> IO Machine
testRunVM is = testRun (toArray $ instMorph <$> is) (return . setMain . setCounter 0 . prepare is $ initMachine)

testRun :: Array PC (Machine -> Machine) -> IO Machine -> IO Machine
testRun is m = do m' <- m
                  let pc = fromInteger . takePC $ m'
                      end = ((snd . bounds) is ==)
                  input <- runDebugger pc m'
                  if input == "exit" || end pc
                    then m
                    else testRun is (return (is ! pc $ m'))

runDebugger :: PC -> Machine -> IO String
runDebugger pc m = do putStrLn $ "[DEUBG] Now in " ++ show pc ++ " | ds: " ++ show ds ++ " | input command"
                      putStr "> "
                      input <- getLine
                      case input of
                          "print m" -> print m >> runDebugger pc m
                          "print ds" -> print (takeDS m) >> runDebugger pc m
                          "print mem" -> print (takeMem m) >> runDebugger pc m
                          "print cs" -> print (takeCS m) >> runDebugger pc m
                          "print labels" -> print (takeL m) >> runDebugger pc m
                          "next" -> return ""
                          "" -> return ""
                          "exit" -> return "exit"
                          _ -> runDebugger pc m
                where ds = takeDS m
