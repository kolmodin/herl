{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module EModule where

-- hp
import qualified Data.ByteString             as B
import           Data.Int

-- 3rd party
import           Data.Generics.Uniplate.Data
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V

-- this project
import           AtomTable                   (AtomTable)
import qualified AtomTable                   as AT
import           Beam
import           ETerm

data EModule =
  EModule
    { emodName         :: B.ByteString
    , emodModNameAtom  :: AtomNo
    , emodCode         :: Code
    , emodImports      :: Vector (AtomNo {- mod -}, AtomNo {- fun -}, Arity)
    , emodExports      :: [(AtomNo {- fun -}, Arity, Int32 {- label -})]
    , emodLabelToIp    :: [(Int32, Int)]
    , emodLiteralTable :: [Literal]
    , emodFunctions    :: [Lambda]
    } deriving (Show)

newtype Code = Code { unCode :: Vector Op }

instance Show Code where
  show (Code v) = show (V.toList v)
  showsPrec i (Code v) = showsPrec i (V.toList v)
  showList vs = showList (map unCode vs)

opAtIp :: Code -> Int -> Op
opAtIp (Code v) ix = v V.! ix

makeCode :: [Op] -> Code
makeCode ops = Code (V.fromList ops)

beamToModule :: Beam -> AtomTable -> (EModule, AtomTable, Beam)
beamToModule bm0@(Beam _ new_atom_names0 _ _ _ _ _ _) at = (emod, at', bm')
  where
    bm'@(Beam modName _ ops _ imps exps lambdas literal) = transformBi updateAtoms bm0
    new_atom_names = [ name | AtomName name <- new_atom_names0 ]
    at' = AT.merge at (AT.fromList new_atom_names)
    emod = EModule
      { emodName = modName
      , emodModNameAtom = AT.lookupByName at' modName
      , emodCode = makeCode ops
      , emodImports = V.fromList [ (emod, fun, arity) | Import emod fun arity <- imps ]
      , emodExports = [ (fun, arity, label) | Export fun arity label <- exps ]
      , emodLabelToIp = [ (label, ip) | (Label label, ip) <- zip ops [0..] ]
      , emodLiteralTable = literal
      , emodFunctions = lambdas
      }
    updateAtoms (AtomNo nr) = AT.lookupByName at' (new_atom_names !! (fromIntegral nr - 1))
