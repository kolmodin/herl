
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           ETerm
import           Process

main :: IO ()
main = print =<< runBeam ["other.beam"] "fib_slow" [EInteger 21]
