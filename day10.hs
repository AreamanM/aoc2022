main =
  readFile "day10.input"
    >>= mapM_ (putStrLn . renderLine)
      . splitInto 40
      . ([0] ++) -- how to prevent an off by one error 101
      . init
      . concat
      . loadMany 1
      . map words
      . lines

sigStrengths :: [Int] -> [Int] -> [Int]
sigStrengths at instrs = [(instrs !! (idx - 1)) * idx | idx <- at]

renderLine :: [Int] -> String
renderLine = zipWith drawPixel [0 ..]
  where
    drawPixel cycle spriteMidPos =
      let spriteTail = spriteMidPos - 1
          spriteHead = spriteMidPos + 1
       in if cycle <= spriteHead && cycle >= spriteTail then '█' else ' '

loadMany :: Int -> [[String]] -> [[Int]]
loadMany _ [] = []
loadMany regX (instr : instrs) = loaded : loadMany updatedRegX instrs
  where
    loaded = load regX instr
    updatedRegX = last loaded

load :: Int -> [String] -> [Int]
load regX ["noop"] = [regX]
load regX ["addx", n] = [regX, regX + read n]

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = fst parts : splitInto n (snd parts)
  where
    parts = splitAt n xs
