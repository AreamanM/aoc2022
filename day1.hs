{-# LANGUAGE OverloadedStrings #-}

import Data.Either (either)
import Data.List (sort)
import Data.Text qualified as T (splitOn, words)
import Data.Text.IO qualified as TIO (readFile)
import Data.Text.Read (decimal)

main =
  TIO.readFile "day1.input"
    >>= print
      . sum
      . take 3
      . reverse
      . sort
      . map (sum . map (either error fst . decimal) . T.words)
      . T.splitOn "\n\n"
