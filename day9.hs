import Control.Arrow ((&&&))
import Data.List (nub)

main =
  readFile "day9.input"
    >>= print
      . length
      . nub
      . map head
      . concat
      . simulateRopeMoves (createRope part1) -- 2 for part 1 or 10 for part 2
      . map ((head &&& read . last) . words)
      . lines
  where
    part1 = 2
    part2 = 10

type Direction = String

type Coord = (Int, Int)

type Rope = [Coord]

type Move = (Direction, Int)

createRope :: Int -> Rope
createRope = flip replicate (0, 0)

simulateRopeMoves :: Rope -> [Move] -> [[Rope]]
simulateRopeMoves = scanl (simulateRopeMove . last) . return

simulateRopeMove :: Rope -> Move -> [Rope]
simulateRopeMove rope (dir, units) = take (units + 1) $ iterate (`moveRopeByOneUnit` dir) rope

moveRopeByOneUnit :: Rope -> Direction -> Rope
moveRopeByOneUnit [knot] _ = [knot]
moveRopeByOneUnit rope dir = updateRopeBody (init (init rope) ++ [newTailPos]) ++ [newHeadPos]
  where
    headPos = last rope
    tailPos = last (init rope)
    newHeadPos = moveHead headPos dir
    newTailPos = moveTail newHeadPos tailPos

updateRopeBody :: Rope -> Rope
updateRopeBody [knot] = [knot]
updateRopeBody rope = foldr (const ((:) . uncurry moveTail . (last &&& last . init) <*> id)) [] rope

moveHead :: Coord -> Direction -> Coord
moveHead (x, y) "L" = (x - 1, y)
moveHead (x, y) "R" = (x + 1, y)
moveHead (x, y) "U" = (x, y + 1)
moveHead (x, y) "D" = (x, y - 1)
moveHead _ _ = undefined

moveTail :: Coord -> Coord -> Coord
moveTail (hx, hy) (tx, ty)
  | max (abs $ hx - tx) (abs $ hy - ty) <= 1 = (tx, ty) -- taxicab :)
  | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))
