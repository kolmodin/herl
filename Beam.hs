{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Beam
  ( Beam(..)
  , ModuleName
  , Strings
  , AtomName(..)
  , Arity(..)
  , OperandLabl(..)
  , AllocList(..)
  , Import(..)
  , Export(..)
  , Lambda(..)
  , Literal(..)
  , Destination(..)
  , Source(..)
  , SelectList(..)
  , YReg(..)
  , Operand(..)
  , Op(..)
  , readBeamFile
  , getBeam
  , operandToETerm
  ) where

-- hp
import           Control.Applicative
import           Control.Monad
import           Data.Binary            (get)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Data
import           Data.Int
import           Data.Word
import           Debug.Trace
import           Numeric
import           Text.Show              (showListWith)

-- 3rd party
import qualified Codec.Compression.Zlib as Zlib
--import           Text.Show.Pretty

-- project
import           ETerm
import           ExtTerm

data Beam = Beam ModuleName [AtomName] [Op] Strings [Import] [Export] [Lambda] [Literal] deriving (Show, Data, Typeable)
type ModuleName = B.ByteString
type Strings = B.ByteString

newtype AtomName = AtomName B.ByteString deriving (Show, Data, Typeable)
newtype Arity = Arity { unArity :: Int32 } deriving (Show, Eq, Data, Typeable)
newtype OperandLabl = OperandLabl Int32 deriving (Show, Data, Typeable)

newtype AllocList = AllocList { unAllocList :: [(Int32, Int32)] } deriving (Eq, Show, Data, Typeable)

data Import = Import !AtomNo !AtomNo !Arity deriving (Show, Data, Typeable)
data Export = Export !AtomNo !Arity !Int32 deriving (Show, Data, Typeable)
data Lambda =
  Lambda
    { funAtom     :: !AtomNo
    , funArity    :: !Int32
    , funLabel    :: !Int32
    , funIndex    :: !Int32
    , funFreeVars :: !Int32
    , funOldUniq  :: !Int32
    } deriving (Show, Data, Typeable)

data Literal = Literal !ExtTerm deriving (Show, Data, Typeable)

newtype Destination = Destination Operand deriving (Show, Data, Typeable)
newtype Source = Source Operand deriving (Show, Data, Typeable)
newtype SelectList = SelectList [(Operand, OperandLabl)] deriving (Show, Data, Typeable)
newtype YReg = YReg { unYReg :: Int32 } deriving (Eq, Show, Data, Typeable)

data Operand
    = OperandInt !Int32
    | OperandAtom !AtomNo
    | OperandXReg !Int32
    | OperandYReg !Int32
    | OperandLabel !Int32
    | OperandTableLiteral !Int32
    | OperandAllocList ![(Int32, Int32)]
    | OperandFReg !Int32
    | OperandNil
    | OperandSelectList [(Operand, OperandLabl)]
    deriving (Show, Data, Typeable)

operandToETerm :: Operand -> ETerm
operandToETerm (OperandInt int) = EInteger (fromIntegral int)
operandToETerm (OperandAtom no) = EAtom no
operandToETerm op = error $ "operandToETerm: not implemented: " ++ show op

data Op
    = {- 0x01 -} Label !Int32
    | {- 0x02 -} FuncInfo !AtomNo !AtomNo !Int32
    | {- 0x03 -} IntCodeEnd
    | {- 0x04 -} Call !Int32 !OperandLabl
    | {- 0x05 -} CallLast !Int32 !OperandLabl !Int32
    | {- 0x06 -} CallOnly !Int32 !OperandLabl
    | {- 0x07 -} CallExt !Int32 !Int32
    | {- 0x08 -} CallExtLast !Arity !Int32 !Int32
    | {- 0x09 -} Bif0 !(Maybe Int32) !Int32 {-ext_fun_ref-} !Destination
    | {- 0x0c -} Allocate !Int32 !Int32
    | {- 0x0e -} AllocateZero !Int32 !Int32
    | {- 0x07 -} AllocateHeapZero !Int32 !AllocList !Int32
    | {- 0x10 -} TestHeap !AllocList !Int32
    | {- 0x11 -} Init !Destination
    | {- 0x12 -} Deallocate !Int32
    | {- 0x13 -} KReturn
    | {- 0x14 -} Send
    | {- 0x15 -} RemoveMessage
    | {- 0x17 -} LoopRec !OperandLabl !Destination
    | {- 0x18 -} LoopRecEnd !OperandLabl
    | {- 0x19 -} Wait !OperandLabl
    | {- 0x27 -} IsLt !OperandLabl !Source !Source
    | {- 0x28 -} IsGe !OperandLabl !Source !Source
    | {- 0x29 -} IsEq !OperandLabl !Source !Source
    | {- 0x2a -} IsNe !OperandLabl !Source !Source
    | {- 0x2b -} IsEqExact !OperandLabl !Source !Source
    | {- 0x2c -} IsNeExact !OperandLabl !Source !Source
    | {- 0x2d -} IsInteger !OperandLabl !Source --erjang uses Destination?!
    | {- 0x30 -} IsAtom !OperandLabl !Source --erjang uses Destination?!
    | {- 0x39 -} IsTuple !OperandLabl !Destination
    | {- 0x3a -} TestArity !OperandLabl !Destination !Int32
    | {- 0x3b -} SelectVal !Source !OperandLabl !SelectList
    | {- 0x3d -} Jump !OperandLabl
    | {- 0x3e -} Catch !YReg !OperandLabl
    | {- 0x3f -} CatchEnd !YReg
    | {- 0x40 -} Move !Source !Destination
    | {- 0x42 -} GetTupleElement !Source !Int32 !Destination
    | {- 0x45 -} PutList !Source !Source !Destination
    | {- 0x46 -} PutTuple !Int32 !Destination
    | {- 0x47 -} Put !Source
    | {- 0x49 -} IfEnd
    | {- 0x4a -} CaseEnd !Source
    | {- 0x4b -} CallFun !Int32
    | {- 0x4e -} CallExtOnly !Int32 !Int32
    | {- 0x5e -} FClearError
    | {- 0x5f -} FCheckError !OperandLabl
    | {- 0x60 -} FMove !Source Destination
    | {- 0x61 -} FConv !Source !Destination
    | {- 0x65 -} FDiv !OperandLabl !Source !Source !Destination
    | {- 0x67 -} MakeFun2 !Int32
    | {- 0x68 -} Try !YReg !OperandLabl
    | {- 0x69 -} TryEnd !Destination
    | {- 0x6a -} TryCase !Destination
    | {- 0x6c -} Raise !Source !Source
    | {- 0x7d -} GcBif2 !(Maybe Int32) !Int32 !Int32 !Source !Source !Destination
    | {- 0x88 -} Trim !Int32 !Int32
    | {- 0x99 -} Line  !Int32
        deriving (Show, Data, Typeable)

class Opp a where
  readOp :: a -> Get Op

instance Opp Op where
  readOp = return

instance (Arg a, Opp b) => Opp (a -> b) where
  readOp f = readOp =<< (f <$> readArg)

class Arg a where
  readArg :: Get a

instance Arg Int32 where
  readArg = readCodeInteger

instance Arg AtomNo where
  readArg = readAtom

instance Arg Destination where
  readArg = fmap Destination readOperand

instance Arg Source where
  readArg = fmap Source readOperand

instance Arg OperandLabl where
  readArg = readLabel

instance Arg AllocList where
  readArg = readAllocList

instance Arg SelectList where
  readArg = readSelectList

instance Arg Arity where
  readArg = Arity <$> readCodeInteger

instance Arg YReg where
  readArg = readYReg

readBeamFile :: String -> IO Beam
readBeamFile fileName = runGet getBeam <$> BL.readFile fileName

getBeam :: Get Beam
getBeam = do
  "FOR1" <- getByteString 4
  skip 4
  "BEAM" <- getByteString 4
  (modName, atoms) <- getAtoms
  code <- getCode
  strs <- getStrings
  impt <- getImpt
  expt <- getExpt
  funt <- getFunT
  litt <- getLitT
  !_ <- skipper
  --True <- isEmpty
  return (Beam modName atoms code strs impt expt funt litt)

getAtoms :: Get (ModuleName, [AtomName])
getAtoms =
  readTag "Atom" $ do
    numAtoms <- get :: Get Int32
    atoms@(AtomName modName:_) <- replicateM (fromIntegral numAtoms) getAtom
    return (modName, atoms)

getAtom :: Get AtomName
getAtom = do
  len <- getWord8
  AtomName <$> getByteString (fromIntegral len)

getCode :: Get [Op]
getCode =
  readTag "Code" $ do
    skip 20
    decodeInstructions
getStrings :: Get B.ByteString
getStrings =
  readOptionalTagWithSize "StrT" B.empty $ \csize ->
    getByteString (fromIntegral csize)

getImpt :: Get [Import]
getImpt =
  readTag "ImpT" $ do
    noOfEntries <- get :: Get Int32
    replicateM (fromIntegral noOfEntries) $ do
      mod_ <- get :: Get Int32
      fun <- get :: Get Int32
      arity <- get :: Get Int32
      return (Import (AtomNo mod_) (AtomNo fun) (Arity arity))

getExpt :: Get [Export]
getExpt =
  readTag "ExpT" $ do
    noOfEntries <- get :: Get Int32
    replicateM (fromIntegral noOfEntries) $ do
      fun <- get :: Get Int32
      arity <- get :: Get Int32
      label <- get :: Get Int32
      return (Export (AtomNo fun) (Arity arity) label)

getFunT :: Get [Lambda]
getFunT =
  readOptionalTag "FunT" [] $ do
    noOfEntries <- get :: Get Int32
    replicateM (fromIntegral noOfEntries) $ do
      fun <- fmap AtomNo get
      arity <- get
      label <- get
      index <- get
      freeVars <- get
      oldUniq <- get
      return (Lambda fun arity label index freeVars oldUniq)

getLitT :: Get [Literal]
getLitT =
  readOptionalTagWithSize "LitT" [] $ \csize -> do
    skip 4
    block <- getLazyByteString (fromIntegral csize - 4)
    let deflated = Zlib.decompress block
    return $ runGet go deflated
  where
    go = do
      noOfEntries <- get :: Get Int32
      replicateM (fromIntegral noOfEntries) $ do
        len <- get :: Get Int32
        arr <- getLazyByteString (fromIntegral len)
        return (Literal (decodeExtTerm True arr))

decodeInstructions :: Get [Op]
decodeInstructions = do
  e <- isEmpty
  if e
    then return []
    else do
      inst <- decodeInstruction
      -- btrace $ "finished instr " ++ show inst
      insts <- decodeInstructions
      return (inst:insts)

btrace :: (Monad m) => String -> m ()
btrace str = trace str $ return ()

decodeInstruction :: Get Op
decodeInstruction = do
  op <- getWord8
  -- btrace $ showString "working on op 0x" . showHex op $ []
  case op of
    0x01 -> readOp Label
    0x02 -> readOp FuncInfo
    0x03 -> readOp IntCodeEnd
    0x04 -> readOp Call
    0x05 -> readOp CallLast
    0x06 -> readOp CallOnly
    0x07 -> readOp CallExt
    0x08 -> readOp CallExtLast
    0x09 -> readOp =<< (Bif0 <$> readOptionalLabel)
    0x0c -> readOp Allocate
    0x0e -> readOp AllocateZero
    0x0f -> readOp AllocateHeapZero
    0x10 -> readOp TestHeap
    0x11 -> readOp Init
    0x12 -> readOp Deallocate
    0x13 -> readOp KReturn
    0x14 -> readOp Send
    0x15 -> readOp RemoveMessage
    0x17 -> readOp LoopRec
    0x18 -> readOp LoopRecEnd
    0x19 -> readOp Wait
    0x27 -> readOp IsLt
    0x28 -> readOp IsGe
    0x29 -> readOp IsEq
    0x2a -> readOp IsNe
    0x2b -> readOp IsEqExact
    0x2c -> readOp IsNeExact
    0x2d -> readOp IsInteger
    0x30 -> readOp IsAtom
    0x39 -> readOp IsTuple
    0x3a -> readOp TestArity
    0x3b -> readOp SelectVal
    0x3d -> readOp Jump
    0x3e -> readOp Catch
    0x3f -> readOp CatchEnd
    0x40 -> readOp Move
    0x42 -> readOp GetTupleElement
    0x45 -> readOp PutList
    0x46 -> readOp PutTuple
    0x47 -> readOp Put
    0x49 -> readOp IfEnd
    0x4a -> readOp CaseEnd
    0x4b -> readOp CallFun
    0x4e -> readOp CallExtOnly
    0x5e -> readOp FClearError
    0x5f -> readOp FCheckError
    0x60 -> readOp FMove
    0x61 -> readOp FConv
    0x65 -> readOp FDiv
    0x67 -> readOp MakeFun2
    0x68 -> readOp Try
    0x69 -> readOp TryEnd
    0x6a -> readOp TryCase
    0x6c -> readOp Raise
    0x88 -> readOp Trim
    0x99 -> readOp Line
    0x7d -> do
      optLabel <- readOptionalLabel
      readOp (GcBif2 optLabel)
    _ -> do
      next <- getByteString 10 <|> return B.empty
      error $
        showString "unknown op code: 0x" .
        showHex op . showString " named " .
        showString (opNames !! fromIntegral op) .
        showString ", next 10 bytes: " .
        showListWith showHex (B.unpack next) $ []

readAtom :: Get AtomNo
readAtom = do
  operand <- readOperand
  case operand of
    OperandAtom a -> return a

readCodeInteger :: Get Int32
readCodeInteger = do
  w <- getWord8
  let t = w .&. 7
  if t == codeInt4_tag
    then readSmallIntValue w
    else error (showString "not a code int: " . showHex w $ [])

codeInt4_tag :: Word8
codeInt4_tag = 0

codeInt12_tag :: Word8
codeInt12_tag = 8

readSmallIntValue :: Word8 -> Get Int32
readSmallIntValue w0 = do
  let !tag = w0 .&. 0x0F
      !hdata = w0 `shiftR` 4
  case () of
    _ | tag .&. 0x08 == 0 -> return (fromIntegral hdata)
    _ | hdata .&. 1 == 0 -> do -- need 1 more byte
          w1 <- getWord8
          let w0' = fromIntegral (hdata `shiftL` 7)
          let w = (w0' + fromIntegral w1) :: Word32
          -- !_ <- btrace ("w0=" ++ show w0 ++ ", w1=" ++ show w1)
          -- !_ <-btrace ("readSmallIntValue: tag=" ++ show tag ++ ", hdata=" ++ show hdata ++ ", w0=" ++ show w0 ++ ", w1= " ++ show w1 ++ ", w=" ++ show w)
          return (fromIntegral w)
    _ -> do
      let len = 2 + (hdata `shiftR` 1)
      bytes <- getByteString (fromIntegral len)
      -- big endian
      let !value = B.foldl' (\acc new -> (acc `shiftL` 8) + fromIntegral new) 0 bytes :: Word32
      return (fromIntegral value)
      --error $
      --   showString "unimplemented case of readSmallIntValue; w0=0x" . showHex w0 .
      --   showString ", tag=0x" . showHex tag .
      --   showString ", hdata=0x" . showHex hdata .
      --   showString ", len=" . shows len .
      --   showString ", data=" . shows (B.unpack bytes) .
      --   showString ", value=" . shows value $ []

readOperand :: Get Operand
readOperand = do
  w0 <- getWord8
  let !tag = w0 .&. 0x07
  case tag of
    0x00 -> OperandInt <$> readSmallIntValue w0
    0x01 ->
      if w0 .&. 0x08 == 0
        then fmap OperandInt (readSmallIntValue w0)
        else do
          let hdata = w0 `shiftR` 4
          if (hdata .&. 0x01) == 0
            then do
              w1 <- getWord8
              let w = ((fromIntegral hdata) `shiftL` 7) + fromIntegral w1
              return (OperandInt w)
            else do
              let len | hdata < 15 = return $ 2 + (fromIntegral hdata `shiftR` 1)
                      | otherwise = do
                          w2 <- fmap fromIntegral readCodeInteger
                          return $ 2 + (fromIntegral hdata `shiftR` 1) + w2
              bytes <- len >>= getByteString
              let !value = B.foldl' (\acc new -> (acc `shiftL` 8) + fromIntegral new) 0 bytes
              let _types = value :: Word32
              return (OperandInt (fromIntegral value))
    0x02 ->
      (\no -> if no == 0
                then OperandNil
                else OperandAtom (AtomNo no)) <$> readSmallIntValue w0
    0x03 -> OperandXReg <$> readSmallIntValue w0
    0x04 -> OperandYReg <$> readSmallIntValue w0
    0x05 -> OperandLabel <$> readSmallIntValue w0
    0x07 -> do -- extended
      let moretag = w0 `shiftR` 4
      case moretag of
        0x01 -> OperandSelectList <$> do
	  noOfEntries <- readCodeInteger
	  -- assert noOfEntries % 2 == 0
	  replicateM (fromIntegral (noOfEntries `div` 2)) $
	    (,) <$> readOperand <*> readLabel
        0x02 -> OperandFReg <$> (getWord8 >>= readSmallIntValue)
        0x03 -> OperandAllocList <$> do
          noOfEntries <- readCodeInteger
          replicateM (fromIntegral noOfEntries) ((,) <$> readCodeInteger <*> readCodeInteger)
        0x04 -> OperandTableLiteral <$> (getWord8 >>= readSmallIntValue)
        _ -> error $ showString "unimplemneted extended operand (tag == 0x" . showHex tag . showString ", moretag == 0x" . showHex moretag . showString ")" $ []
    0x13 -> OperandLabel <$> readSmallIntValue w0
    _ -> error $ showString "unknown operand 0x" . showHex w0 . showString " with tag 0x" . showHex tag $ []

readOptionalLabel :: Get (Maybe Int32)
readOptionalLabel = optLabel <|> return Nothing
  where
    optLabel = do
      operand <- readOperand
      case operand of
        OperandLabel nr -> return (Just nr)
        _ -> fail "not a label" -- unconsumes input

readLabel :: Get OperandLabl
readLabel = do
  op <- readOperand
  case op of
    OperandLabel lbl -> return (OperandLabl lbl)
    _ -> error ("readLabel confused, " ++ show op)

readAllocList :: Get AllocList
readAllocList = do
  op <- readOperand
  case op of
    OperandAllocList list -> return (AllocList list)
    OperandInt int -> return (AllocList [(0, int)])
    _ -> error ("readAllocList confused; " ++ show op)

readSelectList :: Get SelectList
readSelectList = do
  op <- readOperand
  case op of
    OperandSelectList lst -> return (SelectList lst)
    _ -> error ("readSelectList confused; " ++ show op)

readYReg :: Get YReg
readYReg = do
  op <- readOperand
  case op of
    OperandYReg reg -> return $! (YReg reg)
    _ -> error ("readYreg confused; " ++ show op)

readTag :: B.ByteString -> Get b -> Get b
readTag tag dec = readOptionalTagWithSize tag (error "readTag") (const dec)

readOptionalTag :: B.ByteString -> b -> Get b -> Get b
readOptionalTag tag fallback dec =
  readOptionalTagWithSize tag fallback (const dec)

readOptionalTagWithSize :: B.ByteString -> b -> (Int32 -> Get b) -> Get b
readOptionalTagWithSize tag fallback dec = do
  tagMatch <- readTag' <|> return False
  case tagMatch of
    False -> return fallback
    True -> do
      csize <- get :: Get Int32
      block <- getLazyByteString (fromIntegral csize)
      -- account for padding
      let padding = (4 - (fromIntegral csize `mod` 4)) `mod` 4
      skip padding
      return (runGet (dec csize) block)
  where
    readTag' = do
      tag' <- getByteString 4
      unless (tag == tag') $ do
        !_ <- btrace ("expected other tag, got " ++ show tag' ++ " but wanted " ++ show tag)
        fail "wrong tag"
      return True

skipTag :: B.ByteString -> Get ()
skipTag tag = go <|> btrace ("no tag: " ++ show tag)
  where
  go =
    readOptionalTagWithSize tag () $ \ csize -> do
      _bs <- getByteString (fromIntegral csize)
      return ()

skipper :: Get ()
skipper = do
  !_ <- skipTag "LocT"
  !_ <- skipTag "Attr"
  !_ <- skipTag "CInf"
  !_ <- skipTag "Abst"
  !_ <- skipTag "Line"
  return ()

opNames :: [String]
opNames = ["NOP","label/1","func_info/3","int_code_end/0","call/2","call_last/3","call_only/2","call_ext/2","call_ext_last/3","bif0/2","bif1/4", "bif2/5","allocate/2","allocate_heap/3","allocate_zero/2","allocate_heap_zero/3","test_heap/2","init/1","deallocate/1","return/0","send/0", "remove_message/0","timeout/0","loop_rec/2","loop_rec_end/1","wait/1","wait_timeout/2","m_plus/4","m_minus/4","m_times/4","m_div/4","int_div/4", "int_rem/4","int_band/4","int_bor/4","int_bxor/4","int_bsl/4","int_bsr/4","int_bnot/3","is_lt/3","is_ge/3","is_eq/3","is_ne/3","is_eq_exact/3", "is_ne_exact/3","is_integer/2","is_float/2","is_number/2","is_atom/2","is_pid/2","is_reference/2","is_port/2","is_nil/2","is_binary/2","is_constant/2", "is_list/2","is_nonempty_list/2","is_tuple/2","test_arity/3","select_val/3","select_tuple_arity/3","jump/1","catch/2","catch_end/1","move/2","get_list/3", "get_tuple_element/3","set_tuple_element/3","put_string/3","put_list/3","put_tuple/2","put/1","badmatch/1","if_end/0","case_end/1", "call_fun/1","make_fun/3","is_function/2","call_ext_only/2","bs_start_match/2","bs_get_integer/5","bs_get_float/5","bs_get_binary/5", "bs_skip_bits/4","bs_test_tail/2","bs_save/1","bs_restore/1","bs_init/2","bs_final/2","bs_put_integer/5","bs_put_binary/5","bs_put_float/5", "bs_put_string/2","bs_need_buf/1","fclearerror/0","fcheckerror/1","fmove/2","fconv/2","fadd/4","fsub/4","fmul/4","fdiv/4","fnegate/3","make_fun2/1", "try/2","try_end/1","try_case/1","try_case_end/1","raise/2","bs_init2/6","bs_bits_to_bytes/3","bs_add/5","apply/1","apply_last/2","is_boolean/2", "is_function2/3","bs_start_match2/5","bs_get_integer2/7","bs_get_float2/7","bs_get_binary2/7","bs_skip_bits2/5","bs_test_tail2/3","bs_save2/2", "bs_restore2/2","gc_bif1/5","gc_bif2/6","bs_final2/2","bs_bits_to_bytes2/2","put_literal/2","is_bitstr/2","bs_context_to_binary/1","bs_test_unit/3", "bs_match_string/4","bs_init_writable/0","bs_append/8","bs_private_append/6","trim/2","bs_init_bits/6","bs_get_utf8/5","bs_skip_utf8/4","bs_get_utf16/5", "bs_skip_utf16/4","bs_get_utf32/5","bs_skip_utf32/4","bs_utf8_size/3","bs_put_utf8/3","bs_utf16_size/3","bs_put_utf16/3","bs_put_utf32/3", "on_load/0","recv_mark/1","recv_set/1","gc_bif3/7"]
