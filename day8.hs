import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.List (transpose)

main =
  readFile "day8.input"
    >>= print
      . ((+) . countVisible <*> countEdges)
      . (map . map) digitToInt
      . lines
  where
    countVisible = uncurry ((sum .) . zipWith ((.) countTrues . zipWith (||))) . (horizontal &&& vertical)
      where
        horizontal = map countVisibleInRow . init . tail
        vertical = transpose . map countVisibleInRow . init . tail . transpose
        countTrues = foldr (bool id succ) 0
    countVisibleInRow = uncurry (zipWith (||)) . (fromLeft &&& fromRight)
      where
        fromLeft = visible . tail
        fromRight = reverse . visible . tail . reverse

countEdges :: (Foldable t) => [t a] -> Int
countEdges xss = 2 * (length xss + length (head xss) - 2)

visible :: [Int] -> [Bool]
visible [] = []
visible [a] = []
visible (a : b : xs)
  | a > maximum (b : xs) = True : visible (b : xs)
  | otherwise = False : visible (b : xs)
