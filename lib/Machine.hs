{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Machine where

import Command
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.IntMap as M
import qualified Data.Vector as V

{- Type definitions -}
data Machine
  = Machine
      Code
      Stack
      InstructionRegister
      ProgramCounter
      BaseAddressRegister
      ObjectCounter
      Heap
      MethodTables
      InputStream
  deriving (Eq)

type Code = V.Vector Command

type Stack = V.Vector Integer

type InstructionRegister = Command

type ProgramCounter = Int

type BaseAddressRegister = Int

type ObjectCounter = Int

type Heap = M.IntMap HeapEntry

data HeapEntry = HeapEntry ClassID RefCounter (V.Vector Integer) deriving (Eq, Show)

type RefCounter = Int

type MethodTables = [(ClassID, MethodTable)]

type MethodTable = [(MethodID, CodeAddress)]

type InputStream = [String]

-- A computation is a monadic action featuring a Machine state and a possible String exception
-- The result might be a string, but it doesn't have to be
type Computation a = StateT Machine (Except String) a

{--}

{- Define lenses for our machine data type -}
code :: Functor f => (Code -> f Code) -> Machine -> f Machine
code f (Machine c s i pc b o h mts input) = (\c' -> Machine c' s i pc b o h mts input) <$> f c

stack :: Functor f => (Stack -> f Stack) -> Machine -> f Machine
stack f (Machine c s i pc b o h mts input) = (\s' -> Machine c s' i pc b o h mts input) <$> f s

iregister :: Functor f => (InstructionRegister -> f InstructionRegister) -> Machine -> f Machine
iregister f (Machine c s i pc b o h mts input) = (\i' -> Machine c s i' pc b o h mts input) <$> f i

programcounter :: Functor f => (ProgramCounter -> f ProgramCounter) -> Machine -> f Machine
programcounter f (Machine c s i pc b o h mts input) = (\pc' -> Machine c s i pc' b o h mts input) <$> f pc

bregister :: Functor f => (BaseAddressRegister -> f BaseAddressRegister) -> Machine -> f Machine
bregister f (Machine c s i pc b o h mts input) = (\b' -> Machine c s i pc b' o h mts input) <$> f b

ocounter :: Functor f => (ObjectCounter -> f ObjectCounter) -> Machine -> f Machine
ocounter f (Machine c s i pc b o h mts input) = (\o' -> Machine c s i pc b o' h mts input) <$> f o

heap :: Functor f => (Heap -> f Heap) -> Machine -> f Machine
heap f (Machine c s i pc b o h mts input) = (\h' -> Machine c s i pc b o h' mts input) <$> f h

mtables :: Functor f => (MethodTables -> f MethodTables) -> Machine -> f Machine
mtables f (Machine c s i pc b o h mts input) = (\mts' -> Machine c s i pc b o h mts' input) <$> f mts

instream :: Functor f => (InputStream -> f InputStream) -> Machine -> f Machine
instream f (Machine c s i pc b o h mts input) = (\input' -> Machine c s i pc b o h mts input') <$> f input

{--}

{- Commonly used state computations -}
throwErr :: String -> Computation a
throwErr e = do
  m <- get
  lift $ throwE e

throwDiagnosticErr :: String -> Computation a
throwDiagnosticErr e = do
  m <- get
  lift $ throwE $ e ++ "\n" ++ show m

controlComm :: String -> Computation a
controlComm e = do
  lift $ throwE $ "CONTROL: " ++ e

loadNextInstruction :: Computation ()
loadNextInstruction = do
  pc <- use programcounter
  prog <- use code
  if isIndexForVector pc prog
    then do
      iregister .= prog V.! pc
      programcounter += 1
    else throwErr "program counter out of range!"

modifyRefCounter :: (Int -> Int) -> Computation ()
modifyRefCounter f = do
  sOld <- use stack
  if null sOld
    then throwErr "stack address out of range"
    else do
      let heapAddr = fromInteger $ V.last sOld
      h <- use heap
      case M.lookup heapAddr h of
        Nothing -> throwErr "heap address out of range!"
        Just (HeapEntry cid refcounter fs) -> do
          let newHeapEntry = HeapEntry cid (f refcounter) fs
          heap .= M.adjust (const newHeapEntry) heapAddr h
          stack .= V.init sOld

jumpTo :: CodeAddress -> Computation ()
jumpTo a = do
  prog <- use code
  if isIndexForVector a prog
    then do
      iregister .= prog V.! a
      programcounter .= a + 1
    else throwErr "code address out of range"

conditionalJumpTo :: Bool -> CodeAddress -> Computation ()
conditionalJumpTo b a = do
  sOld <- use stack
  if null sOld
    then throwErr "stack address out of range"
    else do
      let val = V.last sOld
      case integerToBool val of
        Nothing -> throwErr "value is not a bool"
        Just b' -> do
          if b == b'
            then jumpTo a
            else loadNextInstruction
          stack .= V.init sOld

{--}

{- Utility functions -}
instance Show Machine where
  show (Machine c s i pc b o h mts input) =
    "Machine state:\nCode segment: "
      ++ customShow c
      ++ "\nStack: "
      ++ show s
      ++ "\nInstruction register: "
      ++ show i
      ++ "\nProgram counter: "
      ++ show pc
      ++ "\nBase address register: "
      ++ show b
      ++ "\nObject counter: "
      ++ show o
      ++ "\nHeap: "
      ++ show h
      ++ "\nMethod tables: "
      ++ show mts
      ++ "\nInput stream: "
      ++ show input

-- Show a code segment with position markings
customShow :: Code -> String
customShow cs = "[" ++ showElements 0 cs ++ "]"
  where
    showElements i cs
      | V.null cs = ""
      | i /= 0 = ", " ++ show i ++ ": " ++ show (V.head cs) ++ showElements (i + 1) (V.tail cs)
      | otherwise = show i ++ ": " ++ show (V.head cs) ++ showElements (i + 1) (V.tail cs)

isIndexForVector :: Int -> V.Vector a -> Bool
isIndexForVector i v = 0 <= i && i < V.length v

isHalted :: Machine -> Bool
isHalted m = case view iregister m of
  Halt -> True
  _ -> False

integerToBool :: Integer -> Maybe Bool
integerToBool 0 = Just False
integerToBool 1 = Just True
integerToBool _ = Nothing

boolToInteger :: Bool -> Integer
boolToInteger False = 0
boolToInteger True = 1

createMachine :: [Command] -> Maybe Machine
createMachine cs = createMachineWithInput cs []

createMachineWithInput :: [Command] -> InputStream -> Maybe Machine
createMachineWithInput [] _ = Nothing
createMachineWithInput (c : cs) inputs = Just $ Machine (V.fromList (c : cs)) (V.fromList [0, 0]) c 1 0 0 M.empty [] inputs

combineUnary :: UnaryOperator -> Integer -> Maybe Integer
combineUnary Not 0 = Just 1
combineUnary Not 1 = Just 0
combineUnary _ _ = Nothing

combineBinary :: BinaryOperator -> Integer -> Integer -> Integer
combineBinary op n m = case op of
  Equals -> boolToInteger $ n == m
  Smaller -> boolToInteger $ n < m
  Greater -> boolToInteger $ n > m
  Plus -> n + m
  Minus -> n - m
  Times -> n * m
  Divide -> n `div` m

{--}

{- Core stepper computation
 - This is independent from a concrete computational context like IO to keep things modular
 - The computation will take in inputs through its input stream and output messages as a result of the computation
 -}
step :: Computation (Maybe String)
step = do
  i <- use iregister
  case i of -- to understand the code better, look at the informal semantics in Command.hs
    Reset -> do
      stack .= V.fromList [0, 0]
      bregister .= 0
      ocounter .= 0
      heap .= M.empty
      loadNextInstruction
      return Nothing
    LoadStack n -> do
      sOld <- use stack
      b <- use bregister
      let symAddr = b + 2 + n -- we need the 2 to offset for base and return address on stack
      if isIndexForVector symAddr sOld
        then do
          stack .= V.snoc sOld (sOld V.! symAddr)
          loadNextInstruction
          return Nothing
        else throwErr "LoadStack: stack address out of range!"
    StoreStack n -> do
      sOld <- use stack
      b <- use bregister
      let symAddr = b + 2 + n -- we need the 2 to offset for base and return address on stack
      if isIndexForVector symAddr (V.init sOld) -- the address must be valid in the new stack (with popped top element)
        then do
          stack .= V.init (V.update sOld (V.fromList [(symAddr, V.last sOld)]))
          loadNextInstruction
          return Nothing
        else throwErr "StoreStack: stack address out of range!"
    PushInt n -> do
      sOld <- use stack
      stack .= V.snoc sOld n
      loadNextInstruction
      return Nothing
    LoadHeap i -> do
      sOld <- use stack
      if null sOld
        then throwErr "LoadHeap: stack address out of range"
        else do
          let heapAddr = fromInteger $ V.last sOld
          h <- use heap
          case M.lookup heapAddr h of
            Nothing -> throwErr "LoadHeap: heap address out of range!"
            Just (HeapEntry _ _ fs) -> do
              if isIndexForVector i fs
                then do
                  let val = fs V.! i
                  stack .= V.snoc (V.init sOld) val
                  loadNextInstruction
                  return Nothing
                else throwErr "LoadHeap: referenced a non-existing field!"
    StoreHeap i -> do
      sOld <- use stack
      if V.length sOld < 2
        then throwErr "StoreHeap: stack address out of range"
        else do
          let val = V.last sOld
          let heapAddr = fromInteger $ V.last $ V.init sOld
          h <- use heap
          case M.lookup heapAddr h of
            Nothing -> throwErr "StoreHeap: heap address out of range"
            Just (HeapEntry cid refcounter fs) -> do
              if isIndexForVector i fs
                then do
                  let fsNew = V.update fs (V.fromList [(i, val)])
                  let heapEntryNew = HeapEntry cid refcounter fsNew
                  heap .= M.adjust (const heapEntryNew) heapAddr h
                  stack .= V.init (V.init sOld)
                  loadNextInstruction
                  return Nothing
                else throwErr "StoreHeap: referenced a non-existing field"
    AllocateHeap n id -> do
      mt <- use mtables
      case lookup id mt of
        Nothing -> throwErr "AllocateHeap: referenced a non-existing class"
        Just _ -> do
          let fieldsNew = V.fromList (replicate n 0)
          let heapEntryNew = HeapEntry id 0 fieldsNew
          key <- use ocounter
          h <- use heap
          heap .= M.insert key heapEntryNew h
          ocounter += 1
          sOld <- use stack
          stack .= V.snoc sOld (toInteger key)
          loadNextInstruction
          return Nothing
    IncrementRefCounter -> do
      modifyRefCounter (+ 1)
      loadNextInstruction
      return Nothing
    DecrementRefCounter -> do
      modifyRefCounter (+ (-1))
      loadNextInstruction
      return Nothing
    CreateMethodTable id methods -> do
      mtsOld <- use mtables
      case lookup id mtsOld of
        Just _ -> throwErr "CreateMethodTable: method table already exists"
        Nothing -> do
          mtables .= (id, methods) : mtsOld
          loadNextInstruction
          return Nothing
    Jump a -> do
      jumpTo a
      return Nothing
    JumpIfTrue a -> do
      conditionalJumpTo True a
      return Nothing
    JumpIfFalse a -> do
      conditionalJumpTo False a
      return Nothing
    CallProcedure a n -> do
      prog <- use code
      unless (isIndexForVector a prog) $ throwErr "CallProcedure: code address out of range"
      sOld <- use stack
      when (V.length sOld < n) $ throwErr "CallProcedure: stack address out of range"
      let numCellsBeforeParams = V.length sOld - n
          (start, params) = V.splitAt numCellsBeforeParams sOld
      b <- use bregister
      pc <- use programcounter
      stack .= start V.++ V.fromList (map toInteger [b, pc]) V.++ params
      bregister .= V.length start
      jumpTo a
      return Nothing
    CallMethod i n -> do
      sOld <- use stack
      when (V.length sOld < n + 1) $ throwErr "CallMethod: stack address out of range"
      let numCellsBeforeParams = V.length sOld - (n + 1)
          (start, params) = V.splitAt numCellsBeforeParams sOld
          objAddr = fromInteger $ V.head params
          normalParams = V.tail params
      h <- use heap
      a <- case M.lookup objAddr h of
        Nothing -> throwErr "CallMethod: heap address out of range"
        Just (HeapEntry cid _ _) -> do
          mts <- use mtables
          case lookup cid mts of
            Nothing -> throwErr "CallMethod: referenced invalid class"
            Just mt -> case lookup i mt of
              Nothing -> throwErr "CallMethod: referenced invalid metthod"
              Just a -> return a
      prog <- use code
      unless (isIndexForVector a prog) $ throwErr "CallMethod: code address out of range"
      pc <- use programcounter
      b <- use bregister
      stack .= start V.++ V.fromList (map toInteger [b, pc]) V.++ params
      bregister .= V.length start
      jumpTo a
      return Nothing
    Return returnsSth -> do
      bOld <- use bregister
      sOld <- use stack
      unless (isIndexForVector bOld sOld) $ throwErr "Return: stack address out of range"
      unless (isIndexForVector (bOld + 1) sOld) $ throwErr "Return: stack address out of range"
      -- get rid of current stack frame
      let sNew = V.take bOld sOld
      if returnsSth
        then stack .= V.snoc sNew (V.last sOld)
        else stack .= sNew
      -- set base address register
      let bNew = fromInteger $ sOld V.! bOld :: BaseAddressRegister
      bregister .= bNew
      -- jump back to return address
      let ra = fromInteger $ sOld V.! (bOld + 1) :: StackAddress
      jumpTo ra
      return Nothing
    CombineUnary Not -> do
      sOld <- use stack
      when (null sOld) $ throwErr "CombineUnary: stack address out of range"
      case combineUnary Not $ V.last sOld of
        Nothing -> throwErr "CombineUnary: input value is not a boolean"
        Just n -> stack .= V.snoc (V.init sOld) n
      loadNextInstruction
      return Nothing
    CombineBinary op -> do
      sOld <- use stack
      when (V.length sOld < 2) $ throwErr "CombineBinary: stack address out of range"
      let sndparam = V.last sOld
      let firstparam = V.last $ V.init sOld
      let newBaseStack = V.init $ V.init sOld
      let res = combineBinary op firstparam sndparam
      stack .= V.snoc newBaseStack res
      loadNextInstruction
      return Nothing
    Read -> do
      inputs <- use instream
      when (null inputs) $ controlComm "need input"
      instream .= tail inputs
      let newin = read $ head inputs :: Integer
      sOld <- use stack
      stack .= V.snoc sOld newin
      loadNextInstruction
      return Nothing
    PrintInt -> do
      sOld <- use stack
      when (null sOld) $ throwErr "Print: stack address out of range"
      let toWrite = V.last sOld
      stack .= V.init sOld
      loadNextInstruction
      return $ Just $ show toWrite
    PrintStr msg -> do
      loadNextInstruction
      return $ Just msg
    PrintStrLn msg -> do
      loadNextInstruction
      return $ Just $ msg ++ "\n"
    Halt -> return Nothing

{--}

{- Machine runner implementations for concrete computational contexts -}
run :: [Command] -> IO ()
run = runDefaultIO

runDebug :: [Command] -> IO ()
runDebug = runInteractiveIO

runTest :: [Command] -> InputStream -> Either String String
runTest cs s = case createMachineWithInput cs s of
  Nothing -> Left "invalid machine code"
  Just m -> runAccumulatingOutput m ""
  where
    runAccumulatingOutput m s = case stepTest m of
      Left str -> Left str
      Right (out, m') ->
        if isHalted m'
          then Right $ s ++ out
          else runAccumulatingOutput m' (s ++ out)

stepTest :: Machine -> Either String (String, Machine)
stepTest m = case runExcept $ runStateT step m of
  Left e -> error e
  Right (Nothing, m) -> return ("", m)
  Right (Just s, m) -> return (s, m)

runDefaultIO :: [Command] -> IO ()
runDefaultIO cs = do
  m <- case createMachine cs of
    Nothing -> error "invalid machine code"
    Just m -> return m
  stepIOUntilHalted m
  where
    stepIOUntilHalted :: Machine -> IO ()
    stepIOUntilHalted m = do
      m' <- stepIO m
      if isHalted m'
        then return ()
        else stepIOUntilHalted m'

runInteractiveIO :: [Command] -> IO ()
runInteractiveIO cs = do
  m <- case createMachine cs of
    Nothing -> error "invalid machine code"
    Just m -> return m
  stepInteractivelyUntilHalted m
  where
    stepInteractivelyUntilHalted :: Machine -> IO ()
    stepInteractivelyUntilHalted m = do
      m' <- stepIO m
      if isHalted m'
        then do
          print m'
          putStrLn "Machine halted."
          return ()
        else do
          print m'
          putStrLn "Press enter for next machine step"
          getLine
          stepInteractivelyUntilHalted m'

stepIO :: Machine -> IO Machine
stepIO m = do
  case runExcept $ runStateT step m of
    Left "CONTROL: need input" -> do
      l <- getLine
      let m' = set instream [l] m
      return m'
    Left e -> error e
    Right (Nothing, m) -> return m
    Right (Just s, m) -> do
      putStr s
      return m

{--}