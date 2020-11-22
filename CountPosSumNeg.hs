module CountPosSumNeg where

countPositivesSumNegatives :: Maybe [Int] -> [Int]
countPositivesSumNegatives xs = case xs of
  Just lst -> case partition (>=0) lst of
    (x, y) -> [sum x, sum y]
  Nothing -> []