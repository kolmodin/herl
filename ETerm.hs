{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module ETerm where

import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Data


newtype AtomNo = AtomNo { unAtomNo :: Int32 } deriving (Show, Eq, Data, Typeable)

data ETerm
    = EInteger !Integer
    | ETuple [ETerm]
    | EString B.ByteString
    | ENil
    | EList !ETerm !ETerm
    | EBinary BL.ByteString
    | EAtom !AtomNo
    | EFun
        { funIp :: !Int
        , funFree :: [ETerm]
        , funMod :: B.ByteString
        , etfunArity :: !Int32
        , funName :: B.ByteString
        }
    | ENonValue
    deriving (Show, Eq, Data, Typeable)

isEtermEq :: ETerm -> ETerm -> Bool
isEtermEq = (==)
