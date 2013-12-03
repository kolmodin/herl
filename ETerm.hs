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

isList :: ETerm -> Bool
isList (EList _ _) = True
isList ENil = True
isList (EString _) = True
isList _ = False

toErlangList :: [ETerm] -> ETerm
toErlangList [] = ENil
toErlangList (x:xs) = EList x (toErlangList xs)

fromErlangList :: ETerm -> [ETerm]
fromErlangList (EList hd tl) = hd : fromErlangList tl
fromErlangList ENil = []
fromErlangList (EString bs) = map (EInteger . fromIntegral ) (B.unpack bs)

isAtom :: ETerm -> Bool
isAtom (EAtom _) = True
isAtom _ = False

isNonValue :: ETerm -> Bool
isNonValue ENonValue = True
isNonValue _ = False

isFunction :: ETerm -> Bool
isFunction (EFun {}) = True
isFunction _ = False
