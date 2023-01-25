import Data.Char
import Data.List

main =
  readFile "day5.input"
    >>= print
      . map head
      . parseAndApplyMoves
      . span isCrateData
      . lines
  where
    fmt (x : xs)
      | isSpace x = '-'
      | isAlpha x = x
      | otherwise = fmt xs
    parseAndApplyMoves (layout, arrangement) = doMoves moves crates
      where
        crates = map (filter (/= '-')) $ transpose $ map (map fmt . splitInto 4) layout
        moves = map (map ((+ 0) . read) . filter (all isDigit) . words) $ drop 2 arrangement

doMoves :: [[Int]] -> [String] -> [String]
doMoves = flip $ foldl $ flip doMove

doMove :: [Int] -> [String] -> [[Char]]
doMove [moves, from, to] crates
  | from < to =
      let p1 = take (from - 1) crates
          p2 = drop to crates
          mid = drop from $ take (to - 1) crates
       in p1 ++ [remaining] ++ mid ++ [moved] ++ p2
  | otherwise =
      let p1 = take (to - 1) crates
          p2 = drop from crates
          mid = drop to $ take (from - 1) crates
       in p1 ++ [moved] ++ mid ++ [remaining] ++ p2
  where
    src -- column from which crates will be moved
      | not $ null subl = head subl
      | otherwise = ""
      where
        subl = drop (from - 1) crates

    -- toMove: which crates will be moved
    -- remaining: source after crates have been moved
    (toMove, remaining) = splitAt moves src

    dest -- destination for crates
      | not $ null subl = head subl
      | otherwise = ""
      where
        subl = drop (to - 1) crates

    -- destination after crates have been moved
    moved = toMove ++ dest -- for part 1, reverse toMove before adding
doMove _ _ = undefined

isCrateData :: String -> Bool
isCrateData = elem '['

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = fst parts : splitInto n (snd parts)
  where
    parts = splitAt n xs
