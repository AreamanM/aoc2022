import Data.Char

main = readFile "day3.input" >>= print . sum . map (toPriority . common3) . splitInto 3 . lines
  where
    common3 [xs, ys, zs] = head $ filter (`elem` filter (`elem` xs) ys) zs

halve :: [a] -> ([a], [a])
halve xs = splitAt (fromIntegral m) xs
  where
    m = length xs `div` 2

toPriority :: Char -> Int
toPriority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = fst parts : splitInto n (snd parts)
  where
    parts = splitAt n xs
