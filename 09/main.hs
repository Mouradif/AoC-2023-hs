linearDiff :: Num a => [a] -> [a]
linearDiff xs = zipWith (-) (tail xs) (init xs)

processLine :: [Integer] -> Integer
processLine line | all (== 0) line = 0
processLine line = last line + (processLine $ linearDiff line)

parseLine :: String -> [Integer]
parseLine str = map read $ words str

main = do
  contents <- readFile "input"
  let x = map parseLine (lines contents)
  let y = sum $ map processLine x
  print(y)
