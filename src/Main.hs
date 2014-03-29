import VM

main :: IO ()
main = putStrLn .show . takeResult $ machine
    where machine = runVM instructions

{- test instructions for:
-    a = 3 + 4
--   b = a + 3
--   b - a
-}
instructions :: [Instruction]
instructions = [ Push 3
               , Push 4
               , Add
               , Store 0 -- end first line
               , Read 0
               , Push 3
               , Add
               , Store 1
               , Read 1
               , Read 0
               , Sub ]
