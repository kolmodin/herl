{-# LANGUAGE OverloadedStrings #-}

module AtomTable
  ( AtomTable
  , empty
  , fromList
  , add
  , merge
  , listNames
  , lookupByName
  , lookupByCode
  ) where

import qualified Data.ByteString as B
import           Data.Int
import           Data.List       as List

import           ETerm           (AtomNo (..))

data AtomTable = AT !Int32 [(B.ByteString, AtomNo)] deriving Show

empty :: AtomTable
empty = AT 0 []

fromList :: [B.ByteString] -> AtomTable
fromList lst = merge empty (AT (fromIntegral (length lst)) (zip lst (map AtomNo [0..])))

add :: AtomTable -> B.ByteString -> AtomTable
add at@(AT no xs) name =
  case lookupByNameM at name of
    Just _ -> at
    Nothing -> AT (no+1) ((name, AtomNo no):xs)

merge :: AtomTable -> AtomTable -> AtomTable
merge at1 at2 = foldl' add at1 (listNames at2)

listNames :: AtomTable -> [B.ByteString]
listNames (AT _ xs) = map fst xs

lookupByNameM :: AtomTable -> B.ByteString -> Maybe AtomNo
lookupByNameM (AT _ xs) name = List.lookup name xs

lookupByName :: AtomTable -> B.ByteString -> AtomNo
lookupByName at name =
  case lookupByNameM at name of
    Just no -> no
    Nothing -> error $ "AT.lookupByName: atom not found: " ++ show name

lookupByCodeM :: AtomTable -> AtomNo -> Maybe B.ByteString
lookupByCodeM (AT _ xs) code = List.lookup code (map (\(x,y) -> (y,x)) xs)

lookupByCode :: AtomTable -> AtomNo -> B.ByteString
lookupByCode at no =
  case lookupByCodeM at no of
    Just bs -> bs
    Nothing -> error $ "AT.lookupByCode: atom not found: " ++ show no
