
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Beam
import           EModule
import           ETerm

main :: IO ()
main = print =<< runBeam ["other.beam"] "fib_slow" [EInteger 21]
