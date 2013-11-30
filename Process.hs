{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Process where


-- hp
import           Control.Monad.State.Strict
import qualified Data.ByteString             as B
import qualified Data.Char                   as Char
import           Data.Int
import           Data.List                   (intersperse)
import           Data.Maybe                  (fromJust, listToMaybe,
                                              maybeToList)
import           Text.Show                   (showListWith)

-- 3rd party
import qualified Data.Vector                 as V
import           Text.Show.Pretty

-- this project
import           AtomTable                   (AtomTable)
import qualified AtomTable                   as AT
import qualified AtomTableErlang             as AT
import           Beam
import           EModule
import           ETerm
import           ExtTerm

data Process =
  Process
    { pXreg       :: ![ETerm]
    , pYreg       :: ![ETerm]
    , pIp         :: !Int
    , pEModule    :: !EModule
    , pStack      :: ![StackFrame]
    , pCatches    :: ![CatchContext]
    , pAtomTable  :: AtomTable
    , pFault      :: Maybe AtomNo
    , pStackTrace :: Maybe ETerm
    , pAllModules :: [(AtomNo, EModule)]
    } deriving Show

data StackFrame
  = StackFrame {-# UNPACK #-} !AtomNo {-# UNPACK #-} !Int
    deriving Show

data CatchContext =
  CatchContext
    { catchModule          :: {-# UNPACK #-} !AtomNo
    , catchIp              :: {-# UNPACK #-} !Int
    , catchCallStackLength :: {-# UNPACK #-} !Int
    , catchActiveYRegs     :: {-# UNPACK #-} !Int
    } deriving Show

runBeam :: [String] -> B.ByteString -> [ETerm] -> IO ()
runBeam files fun args = do
  beams <- mapM readBeamFile files
  let (at, emods) = foldr (\beam (at,emods) -> let (emod, at', beam') = beamToModule beam at in (at', emod:emods)) (AT.basic,[]) beams
      funNameAtom = AT.lookupByName at fun
      p0 = makeProcess emods at funNameAtom args
      stepper = do
        p <- get
        liftIO $ putStrLn $ "X: " ++ showListWith showString (map (renderETerm at) (pXreg p)) []
        liftIO $ putStrLn $ "Y: " ++ showListWith showString (map (renderETerm at) (pYreg p)) []
        liftIO $ putStrLn $ "Catches: " ++ ppShow (pCatches p)
        liftIO $ putStrLn $ "Stack: " ++ ppShow (pStack p)
        liftIO $ putStrLn ""
        liftIO $ putStrLn $ show (pIp p) ++ ": " ++ show (opAtIp (emodCode (pEModule p)) (pIp p))
        liftIO $ putStrLn ""
        r <- step
        case r of
          Nothing -> do
            ps <- stepper
            return (Right (opAtIp (emodCode (pEModule p)) (pIp p), p) : ps)
          Just v -> do
            return [Right (opAtIp (emodCode (pEModule p)) (pIp p), p), Left v]
  putStrLn $ ppShow $ pAtomTable p0
  (steps, _p') <- runStateT stepper p0
  let val = head $ [ v | Left v <- steps ]
  --putStrLn $ ppShow $ beam'
  let rendered = renderETerm at val
  liftIO $ putStrLn rendered

makeProcess :: [EModule] -> AtomTable -> AtomNo -> [ETerm] -> Process
makeProcess emods at fun args =
  let argsArity = fromIntegral (length args)
      (emod, ip) = case [ (emod, ip) | emod <- emods
                                     , (fun', Arity arity, label) <- emodExports emod
                                     , fun' == fun
                                     , arity == argsArity
                                     , ip <- maybeToList (lookup label (emodLabelToIp emod)) ] of
                     [res] -> res
                     _ -> error $ showString "no such label: " . showFA at fun (Arity argsArity) $ []
  in Process args [] ip emod [] [] at Nothing Nothing [ (emodModNameAtom emod, emod) | emod <- emods ]

showByteString :: B.ByteString -> ShowS
showByteString bs = showString (map (Char.chr . fromIntegral) (B.unpack bs))

showMFA :: AtomTable -> AtomNo -> AtomNo -> Arity -> ShowS
showMFA at m0 f0 a0 = showByteString m . showString ":" . showByteString f . showString "/" . shows a
  where
    m = AT.lookupByCode at m0
    f = AT.lookupByCode at f0
    a = unArity a0

showFA :: AtomTable -> AtomNo -> Arity -> ShowS
showFA at f0 a0 = showByteString f . showString "/" . shows a
  where
    f = AT.lookupByCode at f0
    a = unArity a0

type PM a = StateT Process IO a

getProcess :: PM Process
getProcess = get

getsProcess :: (Process -> a) -> PM a
getsProcess f = gets f

modifyProcess :: (Process -> Process) -> PM ()
modifyProcess f = modify f

runBif :: AtomNo -> AtomNo -> Arity -> PM ()
runBif emod fun arity =
  case lookup (emod, fun, arity) bifs of
    Just func -> func
    Nothing ->  error "bif not found"

bifs :: [((AtomNo, AtomNo, Arity), PM ())]
bifs = [ ((AT.am_erlang, AT.am_now, Arity 0), erlangNow0)
       , ((AT.am_erlang, AT.am_throw, Arity 1), erlangThrow1)
       , ((AT.am_erlang, AT.am_exit, Arity 1), erlangExit1)
       , ((AT.am_erlang, AT.am_error, Arity 1), erlangError1)
       , ((AT.am_erlang, AT.am_get_stacktrace, Arity 0), erlangGetStacktrace0)
       ]

erlangNow0 :: PM ()
erlangNow0 =
  writeDestination (Destination (OperandXReg 0)) $
    ETuple [EInteger 0, EInteger 0, EInteger 0]

erlangThrow1 :: PM ()
erlangThrow1 = do
  liftIO $ putStrLn "executing erlang:throw/1"
  t <- readSource (Source (OperandXReg 0))
  fault t ExcThrown
  return ()

erlangExit1 :: PM ()
erlangExit1 = do
  liftIO $ putStrLn "executing erlang:exit/1"
  t <- readSource (Source (OperandXReg 0))
  fault t ExcExit
  return ()

erlangError1 :: PM ()
erlangError1 = do
  liftIO $ putStrLn "executing erlang:error/1"
  t <- readSource (Source (OperandXReg 0))
  fault t (ExcError JustError) --TODO: justerror?
  return ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

data ErrorType
  = ExcError !ErrorKind
  | ExcThrown
  | ExcExit
  deriving (Show)

data ErrorKind
  = JustError
  | BasicError
  | BadMatch
  | CaseClause
  | TryClause
  | BadFun
  | BadArity
  | IfClause
  | FunctionClause
  deriving (Show)

fault :: ETerm -> ErrorType -> PM (Maybe a)
fault errorTerm0 kind0 = do
  st <- stackTrace
  catches <- getsProcess pCatches
  let (errorTerm', kind) = case (catches, kind0) of
        ([], ExcThrown) -> (ETuple [(EAtom AT.am_nocatch), errorTerm0], ExcError BasicError)
        _ -> (errorTerm0, kind0)
      errorTerm = expandErrorTerm errorTerm' kind
  case catches of
    [] -> do
      at <- getsProcess pAtomTable
      liftIO $ putStrLn (renderETerm at errorTerm)
      liftIO $ print kind
      error "fault: should kill process"
    (c:_cs) -> do
      modifyProcess (\p -> p { pStack = reverse . take (catchCallStackLength c) . reverse $ pStack p
                             , pYreg =  reverse . take (catchActiveYRegs     c) . reverse $ pYreg p })
      let st' = ETuple [errorTerm, st]
      writeDestination (Destination (OperandXReg 0)) ENonValue
      writeDestination (Destination (OperandXReg 1)) (EAtom (exceptionAtom kind))
      writeDestination (Destination (OperandXReg 2)) errorTerm
      writeDestination (Destination (OperandXReg 3)) st' --approx...
      modifyProcess (\p -> p { pStackTrace = Just st' })
      gotoModIp (catchModule c) (catchIp c)
      return Nothing

expandErrorTerm :: ETerm -> ErrorType -> ETerm
expandErrorTerm t err =
  case err of
    ExcThrown -> t
    ExcExit -> t
    ExcError BasicError -> t
    ExcError BadMatch -> ETuple [EAtom AT.am_badmatch, t]
    ExcError CaseClause -> ETuple [EAtom AT.am_case_clause, t]
    ExcError TryClause -> ETuple [EAtom AT.am_try_clause, t]
    ExcError BadFun -> ETuple [EAtom AT.am_badfun, t]
    ExcError BadArity -> ETuple [EAtom AT.am_badarity, t]
    ExcError _ -> t

exceptionAtom :: ErrorType -> AtomNo
exceptionAtom (ExcError _) = AT.am_error
exceptionAtom ExcExit = AT.am_exit
exceptionAtom ExcThrown = AT.am_throw

erlangGetStacktrace0 :: PM ()
erlangGetStacktrace0 = do
  st <- getsProcess pStackTrace
  let term = case st of
               Nothing -> ENil
               Just st' -> st'
  writeDestination (Destination (OperandXReg 0)) term
  handleOp KReturn
  return ()

stackTrace :: PM ETerm
stackTrace = do
  stack <- getsProcess pStack
  allModules <- getsProcess pAllModules
  lst <- forM stack $ \(StackFrame modName ip) -> do
    let Just emod = lookup modName allModules
    let code = emodCode emod
    let Just (stackFrame@(emod, fun, ar)) = ipToFunction code ip
    return $ ETuple [EAtom emod, EAtom fun, EInteger (fromIntegral ar)]
  return (toErlangList lst)

ipToFunction :: Code -> Int -> Maybe (AtomNo, AtomNo, Int32)
ipToFunction _ (-1) = Nothing
ipToFunction code n =
  case opAtIp code n of
    FuncInfo emod fun arity -> Just (emod, fun, arity)
    _ -> ipToFunction code (n-1)

renderETerm :: AtomTable -> ETerm -> String
renderETerm at eterm = go at eterm
  where
    go _ ENonValue = "the-non-value"
    go _ (EInteger n) = show n
    go at (ETuple lst) = concat ("{" : intersperse "," (map (go at) lst) ++ ["}"])
    go at (EList hd tl) = "[" ++ concat (intersperse "," (map (go at) (fromList hd tl))) ++ "]"
    go _ (EString str) = show str
    go _ ENil = "nil"
    go _ (EBinary _) = "<<bin>>"
    go at (EAtom no) = showByteString (AT.lookupByCode at no) []
    go _ (EFun {..}) = showByteString funMod . showString ":" . showByteString funName . showString "/" . shows etfunArity $ []

    showAtomName :: String -> String
    showAtomName name
      | any Char.isSpace name = quote name
      | Just c <- listToMaybe name, Char.isUpper c = quote name
      | otherwise = name

    quote :: String -> String
    quote name = "'" ++ name ++ "'"

    fromList :: ETerm -> ETerm -> [ETerm]
    fromList hd ENil = [hd]
    fromList hd (EList hd' tl) = hd : fromList hd' tl

continue :: PM (Maybe a)
continue = modifyProcess (\p -> p { pIp = pIp p + 1 }) >> return Nothing

gotoModIp :: AtomNo -> Int -> PM ()
gotoModIp modName ip = do
  p <- getProcess
  emod <- case lookup modName (pAllModules p) of
            Just emod -> return emod
            Nothing -> error $ "no such module; " ++ show modName
  modifyProcess (\p -> p { pEModule = emod, pIp = ip })

gotoModFunArity :: AtomNo -> AtomNo -> Arity -> PM ()
gotoModFunArity mod fun ar = do
  erl <- lookupModFunArity mod fun ar
  case erl of
    Just (emod, ip) -> gotoModIp (emodModNameAtom emod) ip
    Nothing -> do -- can be a bif
      case lookup (mod, fun, ar) bifs of
        Just func -> func
        Nothing -> do -- TODO: module:fun/ar not found! create erlang error.
          at <- getsProcess pAtomTable
          error $ showString "function " . showMFA at mod fun ar . showString " not found" $ []

lookupModFunArity :: AtomNo -> AtomNo -> Arity -> PM (Maybe (EModule, Int))
lookupModFunArity modName funName funArity = do
  p <- getProcess
  case [ (emod, ip)
       | emod <- maybeToList (lookup modName (pAllModules p))
       , (fun', arity, label) <- emodExports emod
       , fun' == funName
       , arity == funArity
       , ip <- maybeToList (lookup label (emodLabelToIp emod)) ] of
    [res] -> return (Just res)
    _ -> return Nothing

readSource :: Source -> PM ETerm
readSource (Source (OperandInt no)) = return $ EInteger (fromIntegral no)
readSource (Source (OperandXReg n)) =
  getsProcess $ \p -> pXreg p !! (fromIntegral n)
readSource (Source (OperandYReg n)) =
  getsProcess $ \p -> pYreg p !! (fromIntegral n)
readSource (Source (OperandTableLiteral no)) = do
  p <- getProcess
  let Literal x = emodLiteralTable (pEModule p) !! (fromIntegral no)
  return $ extTermToETerm x
readSource (Source (OperandAtom atomNo)) = return $! EAtom atomNo
readSource (Source OperandNil) = return ENil
readSource (Source src) = error $ "readSource " ++ show src

writeDestination :: Destination -> ETerm -> PM ()
writeDestination dest !value =
  case dest of
    (Destination (OperandXReg n)) -> modifyProcess (\p -> p { pXreg = updateList (pXreg p) (fromIntegral n) })
    (Destination (OperandYReg n)) -> modifyProcess (\p -> p { pYreg = updateList (pYreg p) (fromIntegral n) })
    _ -> error ("writeDest " ++ show dest ++ ", value=" ++ show value)
  where
    updateList list pos =
      prefix ++ fill ++ value : drop (pos+1) list
      where
        prefix = take pos list
        fill | length prefix < pos = replicate (pos - length prefix) ENil
             | otherwise = []

popCatch :: PM CatchContext
popCatch = do
  ccs <- getsProcess pCatches
  case ccs of
    (c:cs) -> modifyProcess (\p -> p { pCatches = cs }) >> return c
    [] -> error "popCatch: no catchcontexts to pop"

addStackTrace :: ETerm -> ETerm -> PM ETerm
addStackTrace value exc = do
  where_ <- buildStackTrace exc
  return (ETuple [value, where_])

buildStackTrace :: ETerm -> PM ETerm
buildStackTrace exc = return exc

step :: PM (Maybe ETerm)
step = do
  p <- getProcess
  let op = opAtIp (emodCode (pEModule p)) (pIp p)
  handleOp op

handleOp :: Op -> PM (Maybe ETerm)
handleOp op0 = do
  p <- getProcess
  let thisModuleName = emodModNameAtom (pEModule p)
      sameModule ip = StackFrame thisModuleName ip
  case op0 of
    Label _ -> continue
    Line _ -> continue
    TestHeap _ _ -> continue
    Trim n _ -> deallocateY n >> continue
    Deallocate n -> deallocateY n >> continue
    Call _ (OperandLabl label) -> do
      let jumpIp = fromJust $ lookup label (emodLabelToIp (pEModule p))
          returnIp = pIp p + 1
      updateStack (sameModule returnIp:)
      gotoIp jumpIp
      ret
    CallOnly _ label -> gotoLabel label >> ret
    CallFun arityAndReg -> do
      -- Fun fact; the value arityAndReg says both where the function is stored
      -- as well as its arity.
      efun <- readSource (Source (OperandXReg arityAndReg))
      case efun of
        EFun {} -> do
          let jumpIp = funIp efun
              returnIp = pIp p + 1
              functionArity = etfunArity efun
              freeVars = funFree efun
              x_regs | length freeVars > 0 =
                         -- [ ... function arguments ...
                         -- , ... free variables ...
                         -- , ... function gets moved here ...
                         -- , ... rest of X registers ... ]
                         take (fromIntegral functionArity) (pXreg p) ++
                           freeVars ++ [efun] ++ drop (fromIntegral functionArity + length freeVars + 1) (pXreg p)
                     | otherwise = pXreg p
          if (functionArity /= arityAndReg)
            then fault (ETuple [efun, toErlangList (take (fromIntegral functionArity) (pXreg p))]) (ExcError BadArity)
            else do
              updateStack (sameModule returnIp:)
              gotoIp jumpIp
              setXRegs x_regs
              ret
        _ -> fault efun (ExcError BadFun)
    CallExt _arity ix -> do
      let (modName, funName, funArity) = (emodImports (pEModule p)) V.! (fromIntegral ix)
          returnIp = pIp p + 1
      updateStack (sameModule returnIp:)
      gotoModFunArity modName funName funArity
      ret
    CallExtOnly _arity ix -> do
      let (modName, funName, funArity) = (emodImports (pEModule p)) V.! (fromIntegral ix)
      gotoModFunArity modName funName funArity
      ret
    AllocateZero noYregs _ -> do
      modifyProcess (\p -> p { pYreg = replicate (fromIntegral noYregs) (EInteger 0) ++ (pYreg p) })
      continue
    Allocate noYregs _ -> do
      modifyProcess (\p -> p { pYreg = replicate (fromIntegral noYregs) ENil ++ (pYreg p) })
      continue
    IsLt label src1 src2 -> do
      EInteger value1 <- readSource src1
      EInteger value2 <- readSource src2
      let trueIp = pIp p + 1
      falseIp <- lookupIp label
      gotoIp (if value1 < value2 then trueIp else falseIp)
      return Nothing
    IsEqExact label src1 src2 -> do
      value1 <- readSource src1
      value2 <- readSource src2
      let trueIp = pIp p + 1
      falseIp <- lookupIp label
      gotoIp (if isEtermEq value1 value2 then trueIp else falseIp)
      return Nothing
    IsEq label src1 src2 -> handleOp (IsEqExact label src1 src2) -- todo: fix
    IsInteger lbl src -> do
      value <- readSource src
      ip <- case value of
              EInteger _ -> return (pIp p + 1)
              _ -> lookupIp lbl
      gotoIp ip
      return Nothing
    IsAtom lblIfFalse src -> do
      value <- readSource src
      if isAtom value
        then continue
        else gotoLabel lblIfFalse >> ret
    SelectVal src lbl (SelectList lst) -> do
      val <- readSource src
      let newLbl [] = lbl
          newLbl ((op, branch_lbl):xs)
            | isEtermEq val (operandToETerm op) = branch_lbl
            | otherwise = newLbl xs
      newIp <- lookupIp (newLbl lst)
      gotoIp newIp
      ret
    FuncInfo _ _ _ -> fault ENil (ExcError FunctionClause)
    Move src dest -> do
      value <- readSource src
      writeDestination dest value
      continue
    PutList src1 src2 dest -> do
      val1 <- readSource src1
      val2 <- readSource src2
      let lst = EList val1 val2
      writeDestination dest lst
      continue
    PutTuple arity dest -> do
      -- The 'arity' number of following instructions will be 'Put Source' describing where to find
      -- the values to put into the tuple.
      vals <- replicateM (fromIntegral arity) $ do
        continue
        op <- getOp
        case op of
          Put src -> readSource src
          _ -> error "PutTuple; unexpected instruction"
      writeDestination dest (ETuple vals)
      continue
    Put _src -> error "Put; I expected to be part of a PutTuple!"
    GcBif2 _ _ n op1 op2 dest -> do
      let op = case emodImports (pEModule p) V.! (fromIntegral n) of
                 imp | imp == (AT.am_erlang, AT.am_sign_plus, Arity 2) -> bif_plus
                     | imp == (AT.am_erlang, AT.am_sign_minus, Arity 2) -> bif_minus
                     | imp == (AT.am_erlang, AT.am_sign_mult, Arity 2) -> bif_mult
      bif_binop op op1 op2 dest
      continue
    MakeFun2 no -> do
      let funs = emodFunctions (pEModule p)
          fun = funs!!(fromIntegral no)
          numFree = funFreeVars fun
          free = take (fromIntegral numFree) (pXreg p)
          arity = funArity fun - numFree
      funIp <- lookupIp (OperandLabl (funLabel fun))
      let f = EFun
               { funIp = funIp
               , funMod = emodName (pEModule p)
               , funFree = free
               , etfunArity = arity
               , funName = AT.lookupByCode (pAtomTable p) (funAtom fun)
               }
      writeDestination (Destination (OperandXReg 0)) f
      continue
    KReturn
      | null (pStack p) -> return $ Just ((pXreg p) !! 0)
      | otherwise -> do
          let (jumpIp:newStack) = pStack p
              p' = case jumpIp of
                     StackFrame newMod ip
                       | newMod == thisModuleName -> p { pStack = newStack, pIp = ip }
                       | otherwise -> p { pEModule = fromJust (lookup newMod (pAllModules p))
                                        , pStack = newStack
                                        , pIp = ip }
          modifyProcess (const p')
          ret
    Catch (YReg dest) catchLbl -> do
      returnIp <- lookupIp catchLbl
      let catchContext =
            CatchContext
              { catchModule = emodModNameAtom (pEModule p)
              , catchIp = returnIp
              , catchCallStackLength = length (pStack p)
              , catchActiveYRegs = length (pYreg p)
              }
      writeDestination (Destination (OperandYReg dest)) (EString "catch label!")
      updateCatches (catchContext:)
      continue
    Try y l -> handleOp (Catch y l)
    CatchEnd (YReg catchSrc) -> do
      popCatch
      -- TODO: shrink yregs?
      writeDestination (Destination (OperandYReg catchSrc)) ENil
      r <- readSource (Source (OperandXReg 0))
      when (isNonValue r) $ do
        x1 <- readSource (Source (OperandXReg 1))
        if (x1 == EAtom AT.am_throw)
          then writeDestination (Destination (OperandXReg 0)) =<< readSource (Source (OperandXReg 2))
          else do
            when (x1 == EAtom AT.am_error) $ do
              Just st <- getsProcess pStackTrace
              writeDestination (Destination (OperandXReg 2)) st
            x2 <- readSource (Source (OperandXReg 2))
            writeDestination (Destination (OperandXReg 0))
              (ETuple [EAtom AT.am_EXIT, x2])
      continue

    TryEnd dst -> do
      popCatch
      writeDestination dst ENil
      r <- readSource (Source (OperandXReg 0))
      when (isNonValue r) $ do
        readSource (Source (OperandXReg 1)) >>= writeDestination (Destination (OperandXReg 0))
        readSource (Source (OperandXReg 2)) >>= writeDestination (Destination (OperandXReg 1))
        readSource (Source (OperandXReg 3)) >>= writeDestination (Destination (OperandXReg 2))
      continue
    TryCase dst -> handleOp (TryEnd dst)

    {- Faults -}
    CaseEnd nonMatchingClauseSrc -> do -- a case expression has ended without a match.
      t <- readSource nonMatchingClauseSrc
      fault t (ExcError CaseClause)
    IfEnd ->
      fault ENil (ExcError IfClause)

    _ -> error (show op0)
  where
    getOp = getsProcess $ \p -> opAtIp (emodCode (pEModule p)) (pIp p)
    deallocateY n = modifyProcess (\p -> p { pYreg = drop (fromIntegral n) (pYreg p) })
    updateStack f = modifyProcess (\p -> p { pStack = f (pStack p)})
    updateCatches f = modifyProcess (\p -> p { pCatches = f (pCatches p) })
    setXRegs x_regs = modifyProcess (\p -> p { pXreg = x_regs})
    bif_binop f op1 op2 dest = do
      value1 <- readSource op1
      value2 <- readSource op2
      writeDestination dest (f value1 value2)
    bif_plus (EInteger x) (EInteger y) = EInteger (x+y)
    bif_plus x y = error (show x ++ " + " ++ show y)
    bif_minus (EInteger x) (EInteger y) = EInteger (x-y)
    bif_minus x y = error (show x ++ " - " ++ show y)
    bif_mult (EInteger x) (EInteger y) = EInteger (x*y)
    bif_mult x y = error (show x ++ " * " ++ show y)
    lookupIp (OperandLabl label) = do
      p <- getProcess
      case lookup label (emodLabelToIp (pEModule p)) of
        Nothing -> error $ "tried to lookup label that does not exist: " ++ show label
        Just ip -> return ip
    gotoLabel label = gotoIp =<< lookupIp label
    gotoIp ip = modifyProcess (\p -> p { pIp = ip })
    ret = return Nothing
