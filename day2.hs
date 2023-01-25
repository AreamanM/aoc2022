import Control.Arrow ((&&&))

main =
  readFile "day2.input"
    >>= print
      . sum
      . map (uncurry calcPoints . (toMove . head &&& toStrat . last) . words)
      . lines

data Move = Rock | Paper | Scissor

toMove :: String -> Move
toMove "A" = Rock
toMove "B" = Paper
toMove "C" = Scissor
toMove _ = undefined

data Strat = Win | Lose | Draw

toStrat :: String -> Strat
toStrat "X" = Lose
toStrat "Y" = Draw
toStrat "Z" = Win
toStrat _ = undefined

calcPoints :: Move -> Strat -> Int
calcPoints move Lose = (movePoints move + 1) `mod` 3 + 1
calcPoints move Draw = 3 + movePoints move
calcPoints move Win = 6 + (movePoints move - 3) `mod` 3 + 1

movePoints :: Move -> Int
movePoints Rock = 1
movePoints Paper = 2
movePoints Scissor = 3
