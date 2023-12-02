module Lib
  ( displaySolutions
  ) where

displaySolutions :: IO ()
displaySolutions = do 
  let reportIO = map putStrLn reports
  sequence_ reportIO

reports :: [String]
reports = map formatSolution solutions

data Solution a = Solution Int Int a

solutions :: [Solution Integer]
solutions = 
  [ Solution 1 1 123
  ]

formatSolution :: Show a => Solution a -> String
formatSolution (Solution d n s) = d' ++ "." ++ n' ++ ":\t" ++ s'
  where
    d' = show d
    n' = show n
    s' = show s