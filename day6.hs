import Control.Arrow (first, second)
import Control.Monad (join)
import Data.List (findIndex, group, isPrefixOf, sort, tails)
import Data.Tuple (swap)

main =
  readFile "day6.input"
    >>= print
      . fmap (+ 4)
      . uncurry (flip (findIndex . isPrefixOf) . tails)
      . second findUnique
      . join (,)
      . init

findUnique :: String -> String
findUnique [] = ""
findUnique xs
  | dup >= 0 = findUnique $ drop (dup + 1) xs
  | otherwise = take 4 xs
  where
    dup = findDup $ take 4 xs

findDup :: String -> Int
findDup xs
  | not $ null dupes =
      let dupChar = head $ head dupes
       in length $ takeWhile (/= dupChar) xs
  | otherwise = -1
  where
    dupes = filter ((> 1) . length) $ group $ sort xs
