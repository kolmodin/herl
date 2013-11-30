
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           ETerm
import           Process

main :: IO ()
main = print =<< runBeam ["other.beam"] "other" "fib_slow" [EInteger 21]

main2 :: IO ()
main2 = print =<< runBeam ["pingpong.beam"] "pingpong" "start" []
