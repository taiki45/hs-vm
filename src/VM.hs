module VM
    ( Instruction (..)
    , runVM
    , takeResult
    , takePC
    , testRunVM ) where

import Control.Applicative ((<$>))
import Data.Array
import Data.Char
import Data.Maybe
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


--- Bellow is for debugging ---

testRunVM :: [Instruction] -> IO Machine
testRunVM is = testRun (toArray $ instMorph <$> is) (setMain . setCounter 0 . prepare is $ initMachine)

testRun :: Array PC (Machine -> Machine) -> Machine -> IO Machine
testRun = testRun' Nothing

-- TODO: refactoring
testRun' :: Maybe PC -> Array PC (Machine -> Machine) -> Machine -> IO Machine
testRun' bp is m = do let pc = fromInteger . takePC $ m
                          end = ((snd . bounds) is ==)
                      input <- if isNothing bp || bp == Just pc
                                   then runDebugger pc m
                                   else return "next"
                      if input == "exit" || end pc
                        then return m
                        else if bp == Just pc || (isNothing bp && input == "next")
                                 then testRun' Nothing is (is ! pc $ m)
                                 else if any isNumber input
                                    then testRun' (Just . read $ filter isNumber input) is (is ! pc $ m)
                                    else testRun' bp is (is ! pc $ m)

runDebugger :: PC -> Machine -> IO String
runDebugger pc m = do putStrLn $ "[DEUBG] Now in " ++ show pc ++ " | ds: " ++ show ds ++ " | input command"
                      putStr "> "
                      input <- getLine
                      case input of
                          "print m" -> print m >> runDebugger pc m
                          "print ds" -> print (takeDS m) >> runDebugger pc m
                          "print mem" -> print (takeMem m) >> runDebugger pc m
                          "print cs" -> print (takeCS m) >> runDebugger pc m
                          "print lds" -> print (takeLDS m) >> runDebugger pc m
                          "print labels" -> print (takeL m) >> runDebugger pc m
                          "next" -> return "next"
                          "" -> return ""
                          "exit" -> return "exit"
                          ('s':'e':'t':'b':'p':bp) -> return bp
                          _ -> runDebugger pc m
                where ds = takeDS m
