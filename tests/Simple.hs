{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import           System.Exit

import           ETerm
import           Process


main :: IO ()
main = do
  runAssert "test1" [] (EInteger 700)
  runAssert "test2" [] (EInteger 15)
  runAssert "test3" [] (EInteger 64)
  runAssert "inc" [EInteger 1] (EInteger 2)
  runAssert "inc" [EInteger 100] (EInteger 101)
  runAssert "inc" [EInteger 1000] (EInteger 1001)
  runAssert "inc" [EInteger 987654321] (EInteger 987654322)
  runAssert "adder5" (map EInteger [101..105]) (EInteger 515)
  runAssert "test_if" [EInteger 101] (EAtom (AtomNo 31))
  runAssert "test_if" [EInteger 5] (EAtom (AtomNo 30))
  runAssert "test_if" [EInteger 1] (EAtom (AtomNo 29))
  runAssert "test_if" [EInteger 3] (EAtom (AtomNo 28))
  runAssert "simple_fun" [] (EInteger 50)
  runAssert "test_fun" [] (EInteger 92)

runAssert :: B.ByteString -> [ETerm] -> ETerm -> IO ()
runAssert fun args expected = do
  putStrLn $ "Function " ++ show fun ++ " with args " ++ show args
  actual <- runBeam ["tests/simple.beam"] "simple" fun args
  if actual == Just expected
    then putStrLn "ok."
    else do
      putStrLn $ "In " ++ show fun ++ ", " ++ show args ++ ": Expected " ++ show expected ++ " but got " ++ show actual
      exitFailure