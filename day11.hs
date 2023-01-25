{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad (forM_, join)
import Data.IntMap.Strict ((!))
import Data.IntMap.Strict qualified as Map
import Data.List (iterate, partition, sort)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read

main =
  TIO.readFile "day11.input"
    >>= print
      . product
      . take 2
      . reverse
      . sort
      . map (inspected . snd)
      . Map.toList
      . runRounds 8 10_000
      . Map.fromList
      . zip [0 ..]
      . map (parseMonkeySpec . map T.words . T.lines)
      . T.splitOn "\n\n"

data Monkey = Monkey
  { idNum :: Int,
    items :: [Int],
    op :: [T.Text],
    divBy :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    inspected :: Int
  }
  deriving (Show)

parseMonkeySpec :: [[T.Text]] -> Monkey
parseMonkeySpec [idNum, startItems, op, divBy, ifTrue, ifFalse] =
  Monkey
    { idNum = toInt $ last idNum,
      items = map toInt $ drop 2 startItems,
      op = drop 3 op,
      divBy = toInt $ last divBy,
      ifTrue = toInt $ last ifTrue,
      ifFalse = toInt $ last ifFalse,
      inspected = 0
    }
parseMonkeySpec _ = undefined

runRounds :: Int -> Int -> Map.IntMap Monkey -> Map.IntMap Monkey
runRounds monkeyAmount rounds = foldr1 (.) (replicate rounds (runRound monkeyAmount))

runRound :: Int -> Map.IntMap Monkey -> Map.IntMap Monkey
runRound monkeyAmount monkeys = runRound' monkeyAmount monkeys 0
  where
    runRound' monkeyAmount monkeys curr
      | curr == monkeyAmount = monkeys
      | otherwise = runRound' monkeyAmount (runInspection (monkeys ! curr) monkeys) (curr + 1)

runInspection :: Monkey -> Map.IntMap Monkey -> Map.IntMap Monkey
runInspection monkey monkeys =
  let newWorries = map (flip mod 9699690 . parseOp (op monkey)) (items monkey)
      trueMonkeyId = ifTrue monkey
      falseMonkeyId = ifFalse monkey
      trueMonkey = monkeys ! trueMonkeyId
      falseMonkey = monkeys ! falseMonkeyId
      (toTrueMonkey, toFalseMonkey) = partition ((== 0) . (`mod` divBy monkey)) newWorries
   in ( Map.adjust (const monkey {items = [], inspected = inspected monkey + length (items monkey)}) (idNum monkey)
          . Map.adjust (const trueMonkey {items = items trueMonkey ++ toTrueMonkey}) trueMonkeyId
          . Map.adjust (const falseMonkey {items = items falseMonkey ++ toFalseMonkey}) falseMonkeyId
      )
        monkeys

parseOp :: [T.Text] -> (Int -> Int)
parseOp [_, op, operand] =
  case op of
    "+" -> (+ toInt operand)
    "-" -> (`subtract` toInt operand)
    "*" ->
      ( case operand of
          "old" -> (^ 2)
          _ -> (* toInt operand)
      )

toInt :: T.Text -> Int
toInt = either error fst . decimal
