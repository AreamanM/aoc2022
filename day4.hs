{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((&&&), (***))
import Control.Monad (join)
import Data.Bool (bool)
import Data.List (sort)
import Data.Text qualified as T (lines, splitOn)
import Data.Text.IO qualified as TIO (readFile)
import Data.Text.Read (decimal)

main =
  TIO.readFile "day4.input"
    >>= print
      . foldr
        ( bool id succ
            . hasOverlap
            . map (join (***) (either error fst . decimal) . (head &&& last) . T.splitOn "-")
            . T.splitOn ","
        )
        0
      . T.lines
  where
    hasOverlap [(a, b), (c, d)] = b - c >= 0 && d - a >= 0
