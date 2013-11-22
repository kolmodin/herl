{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Data.List             as List
import           Data.Maybe            as Maybe

import           Language.Haskell.Exts hiding (name, sym)


main :: IO ()
main = writeBasic

writeBasic :: IO ()
writeBasic = writeToFile "AtomTableErlang.hs" =<< readTable "atoms.txt"

data AtomName = AN
  { atName :: String -- name without am_ prefix
  , atSymbol   :: String -- symbol
  } deriving (Show, Eq)

readTable :: String -> IO [AtomName]
readTable fileName = Maybe.catMaybes . map mkAtomName . lines <$> readFile fileName

mkAtomName :: String -> Maybe AtomName
mkAtomName "" = Nothing
mkAtomName ('#':_) = Nothing
mkAtomName line =
  case break (==' ') line of
    (name, ' ':sym) -> Just (AN name sym)
    (name, "") -> Just (AN name name)

writeToFile :: String -> [AtomName] -> IO ()
writeToFile fileName names = writeFile fileName (prettyPrint (modu names))

modu :: [AtomName] -> Module
modu names0 =
  Module
    srcloc
    (ModuleName "AtomTableErlang")
    [ LanguagePragma srcloc [ Ident "OverloadedStrings" ] ]
    Nothing
    (exportSpec am_names)
    impDecl
    (mkBasic symbols ++ am_functions am_names)
  where
    names = List.nub names0
    am_names = map (("am_" ++) . atName) names
    symbols = map atSymbol names

exportSpec :: [String] -> Maybe [ExportSpec]
exportSpec names = Just (EVar (UnQual (Ident "basic")) : map (EVar . UnQual . Ident) names)

impDecl :: [ImportDecl]
impDecl =
  [ ImportDecl
     { importLoc = srcloc
     , importModule = ModuleName "AtomTable"
     , importQualified = False
     , importSrc = False
     , importPkg = Nothing
     , importAs = Nothing
     , importSpecs = Nothing
     }
  , ImportDecl
     { importLoc = srcloc
     , importModule = ModuleName "ETerm"
     , importQualified = False
     , importSrc = False
     , importPkg = Nothing
     , importAs = Nothing
     , importSpecs = Just ( False , [ IThingAll (Ident "AtomNo") ] )
     }
  ]

am_functions :: [String] -> [Decl]
am_functions names = concatMap (\(name, ix) -> am_function name ix) (zip names [0..])

am_function :: String -> Integer -> [Decl]
am_function name atomNo =
  [ TypeSig
      srcloc
      [ Ident name ]
      (TyCon (UnQual (Ident "AtomNo")))
  , PatBind
      srcloc
      (PVar (Ident name))
      Nothing
      (UnGuardedRhs (App (Con (UnQual (Ident "AtomNo"))) (Lit (Int atomNo))))
      (BDecls [])]

mkBasic :: [String] -> [Decl]
mkBasic names =
  [ TypeSig
      srcloc
      [ Ident "basic" ]
      (TyCon (UnQual (Ident "AtomTable")))
  , PatBind
      srcloc
      (PVar (Ident "basic"))
      Nothing
      (UnGuardedRhs
         (App
            (Var (UnQual (Ident "fromList")))
            (List lst)))
      (BDecls [])]
 where
  lst = map (Lit . String) names

srcloc :: SrcLoc
srcloc = SrcLoc "" 0 0
