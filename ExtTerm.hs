{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module ExtTerm where

import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Binary.Get
import Data.Binary

import Control.Monad
import Control.Applicative

import Data.Data

import ETerm

data ExtTerm
    = ExtSmallInt !Int8
    | ExtInt !Int32
    | ExtTuple [ExtTerm]
    | ExtString B.ByteString
    | ExtNil
    | ExtList [ExtTerm] ExtTerm
    | ExtBinary BL.ByteString
    | ExtAtom B.ByteString
    deriving (Show, Eq, Data, Typeable)

isExtTermEq :: ExtTerm -> ExtTerm -> Bool
isExtTermEq = (==)

extTermToETerm :: ExtTerm -> ETerm
extTermToETerm e =
  case e of
    ExtSmallInt i -> EInteger (fromIntegral i)
    ExtInt i -> EInteger (fromIntegral i)
    ExtTuple t -> ETuple (map extTermToETerm t)
    ExtString s -> EString s
    ExtNil -> ENil
    ExtList lst tl -> foldr (\hd tl' -> EList (extTermToETerm hd) tl') (extTermToETerm tl) lst
    ExtBinary bin -> EBinary bin
    ExtAtom _ -> error "extTermToETerm: need to be in the PM monad to translate atoms" --  EAtom a

decodeExtTerm :: Bool -> BL.ByteString -> ExtTerm
decodeExtTerm versionNumber input =
  runGet getTerm input
  where
    getTerm = do
      vers
      dec
    vers = do
      when versionNumber $ do
        131 <- getWord8
        return ()
    dec = do
      tag <- getWord8
      case tag of
        97 -> ExtSmallInt <$> get
        98 -> ExtInt <$> get
        100 -> ExtAtom <$> do
          len <- getWord16be
          getByteString (fromIntegral len)
        104 -> ExtTuple <$> (do
          noOfElements <- getWord8
          replicateM (fromIntegral noOfElements) dec)
        105 -> ExtTuple <$> (do
          noOfElements <- getWord32be
          replicateM (fromIntegral noOfElements) dec)
        106 -> return ExtNil
        107 -> ExtString <$> (do
          noOfElements <- getWord16be
          getByteString (fromIntegral noOfElements))
        108 -> do
          noOfElements <- getWord32be
          hd <- replicateM (fromIntegral noOfElements) dec
          tl <- dec
          return $ ExtList hd tl
        109 -> ExtBinary <$> (do
          noOfElements <- getWord32be
          getLazyByteString (fromIntegral noOfElements))
        _ -> error ("decodeExtTerm: unknown tag; " ++ show tag)