{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CodeGenerator where

import Command
import Control.Lens
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import SyntaxTree

{- Basic helper type definitions -}
-- A symbol has a name, can be const and has a type as well as a position in the local variable segment on the stack
data SymbolEntry = SymbolEntry String IsConst Type Position deriving (Eq, Show)

type IsConst = Bool

-- A type can be either a normal primitive integer or an object with a type
data Type = INT | OBJ String deriving (Eq, Show)

-- An optional type is what occurs in an expression hole, since it can be a procedure/method call with no return value
type OptionalType = ReturnType

-- A return type is either empty or a normal type
type ReturnType = Maybe Type

type Position = Int

-- symbol tables are used like a stack whose top is on the front
-- a new symbol on top will shadow old symbols with the same type
type SymTable = [SymbolEntry]

-- A procedure has a signature and an address in the program
data ProcEntry = ProcEntry Signature CodeAddress deriving (Eq, Show)

-- A procedure is either a standard procedure or an initializer/constructor
data ProcKind = NORMAL | INIT deriving (Eq, Show)

-- A signature consists a name, a list that resembles the parameter types and a return type
data Signature = Signature String [Type] ReturnType deriving (Eq, Show)

type ProcTable = [ProcEntry]

type ClassTable = [(ClassID, ClassEntry)]

-- A class has a name, possibly an upper class, a table of fields and a table of methods
data ClassEntry = ClassEntry String (Maybe ClassID) FieldTable MethodTable deriving (Eq, Show)

type FieldTable = [FieldEntry]

-- A field has a name, a type and a position in the heap frame
data FieldEntry = FieldEntry String Type Position deriving (Eq, Show)

type MethodTable = [(MethodID, ProcEntry)]

type PrefixLength = Int -- Denotes the length of the preceding program at certain point

{- The code generator maintains a state that consists of a current prefix
 - as well as a symbol table
 - a procedure table
 - and a class table
 -}
data GenState = GenState PrefixLength SymTable ProcTable ClassTable

-- The typifier essentially shares the code generator's state, but doesn't need to know the current prefix
data TypeState = TypeState SymTable ProcTable ClassTable

-- Every command can be in procedure-, method-, main-program- context or inside of a command block
data CommandContext = PROCEDURE | METHOD | MAIN | INNER

-- Here we define a type for monadic actions that represent the types of our code generators and typifiers
type GeneratorAction a = ExceptT String (State GenState) a

-- A typifier action only needs to read the state, hence the use of Reader instead of State
type TypifierAction a = ExceptT String (Reader TypeState) a

{--}

{- Lens definitions for GenState and TypeState -}
prefixlength :: Functor f => (PrefixLength -> f PrefixLength) -> GenState -> f GenState
prefixlength f (GenState pl st pt ct) = (\pl' -> GenState pl' st pt ct) <$> f pl

symtable :: Functor f => (SymTable -> f SymTable) -> GenState -> f GenState
symtable f (GenState pl st pt ct) = (\st' -> GenState pl st' pt ct) <$> f st

proctable :: Functor f => (ProcTable -> f ProcTable) -> GenState -> f GenState
proctable f (GenState pl st pt ct) = (\pt' -> GenState pl st pt' ct) <$> f pt

classtable :: Functor f => (ClassTable -> f ClassTable) -> GenState -> f GenState
classtable f (GenState pl st pt ct) = (\ct' -> GenState pl st pt ct') <$> f ct

symtablet :: Functor f => (SymTable -> f SymTable) -> TypeState -> f TypeState
symtablet f (TypeState st pt ct) = (\st' -> TypeState st' pt ct) <$> f st

proctablet :: Functor f => (ProcTable -> f ProcTable) -> TypeState -> f TypeState
proctablet f (TypeState st pt ct) = (\pt' -> TypeState st pt' ct) <$> f pt

classtablet :: Functor f => (ClassTable -> f ClassTable) -> TypeState -> f TypeState
classtablet f (TypeState st pt ct) = (\ct' -> TypeState st pt ct') <$> f ct

{--}

{- Type class for syntactical elements that can be compiled -}

-- ContextGeneratable is meant for syntactical elements that can be compiled, but need additional context information to the GenState alone
class ContextGeneratable c a where
  contextGenerator :: c -> a -> GeneratorAction [Command.Command]

-- Generatable is for syntactical elements that can be compiled without additional context (apart from the GenState)
class Generatable a where
  -- A generator creates a monadic action from a syntactical element that can generate code for it
  generator :: a -> GeneratorAction [Command.Command]

  -- This runs a generator with some supplied state (can also be useful for testing)
  customGenerate :: a -> GenState -> Either String [Command.Command]
  customGenerate e s = evalState (runExceptT $ generator e) s

  -- This runs a generator with some default empty state (mostly useful for whole programs)
  generate :: a -> Either String [Command.Command]
  generate e = customGenerate e $ GenState 0 [] [] []

-- A small helper to output the generator state in case of failure
throwDiagnosticError :: String -> GeneratorAction a
throwDiagnosticError s = do
  pl <- use prefixlength
  st <- use symtable
  pt <- use proctable
  ct <- use classtable
  throwE $
    s ++ "\nDiagnostic code generation data: \n"
      ++ "Current prefix length: "
      ++ show pl
      ++ "\nCurrent symbol table:\n"
      ++ show st
      ++ "\nCurrent procedure table:\n"
      ++ show pt
      ++ "\nCurrent class table:\n"
      ++ show ct

{--}

{- Type class for representing syntactical structures whose type can be calculated -}
class Typeable a where
  -- A typifier can typify some object in the context of a TypeState, possibly resulting in a String error
  typifier :: a -> TypifierAction OptionalType

  -- Run a typifier, providing a TypeState
  runTypifier :: a -> TypeState -> Either String OptionalType
  runTypifier e s = runReader (runExceptT $ typifier e) s

  -- An adapter to conveniently run typifiers in the context of generators (look below for the Generatable class)
  typify :: a -> GeneratorAction OptionalType
  typify e = do
    st <- use symtable
    pt <- use proctable
    ct <- use classtable
    case runTypifier e (TypeState st pt ct) of
      Left err -> throwDiagnosticError err
      Right mt -> return mt

{--}

{- Other helper functions -}
replaceClassInTable :: ClassID -> ClassEntry -> ClassTable -> ClassTable
replaceClassInTable id c ct = (id, c) : filter ((/=) id . fst) ct

lookupClassByName :: String -> ClassTable -> Maybe (ClassID, ClassEntry)
lookupClassByName name ct =
  let hits = [hit | hit@(_, ClassEntry cn _ _ _) <- ct, name == cn]
   in if hits /= [] then Just $ head hits else Nothing

isTypeLowerBoundOf :: ClassTable -> [Type] -> [[Type]] -> Bool
isTypeLowerBoundOf ct t = all (areSubtypesOf ct t)

areSubtypesOf :: ClassTable -> [Type] -> [Type] -> Bool
areSubtypesOf ct tls trs = length tls == length trs && all (== True) (zipWith (isSubtypeOf ct) tls trs)

isSubtypeOf :: ClassTable -> Type -> Type -> Bool
isSubtypeOf _ INT INT = True
isSubtypeOf _ INT (OBJ _) = False
isSubtypeOf _ (OBJ _) INT = False
isSubtypeOf ct (OBJ s) (OBJ t) = isSubclassOf ct s t

isSubclassOf :: ClassTable -> ClassName -> ClassName -> Bool
isSubclassOf ct s t =
  s == t || case lookupClassByName s ct of
    Nothing -> False
    Just (_, ClassEntry _ Nothing _ _) -> False
    Just (_, ClassEntry _ (Just tid) _ _) -> case lookup tid ct of
      Nothing -> False
      Just (ClassEntry s' _ _ _) -> isSubclassOf ct s' t

paramToType :: FormalParameterDeclaration -> Type
paramToType (VarParameter _) = INT
paramToType (ObjectParameter (Object t _)) = OBJ t

addParamsToSymbols :: SymTable -> FormalParameterList -> Maybe SymTable
addParamsToSymbols st [] = Just st
addParamsToSymbols st (p : ps) = do
  st' <- addParamToSymbols st p
  addParamsToSymbols st' ps
  where
    addParamToSymbols :: SymTable -> FormalParameterDeclaration -> Maybe SymTable
    addParamToSymbols st (VarParameter (Var n)) = addSymbol st n False INT
    addParamToSymbols st (ObjectParameter (Object t n)) = addSymbol st n False (OBJ t)

addSymbol :: SymTable -> String -> IsConst -> Type -> Maybe SymTable
addSymbol st name isconst t =
  if shadowsConst st name
    then Nothing
    else Just $ SymbolEntry name isconst t (length st) : st
  where
    shadowsConst st name = case lookupSymbol st name of
      Nothing -> False
      Just (SymbolEntry _ False _ _) -> False
      Just (SymbolEntry _ True _ _) -> True

lookupSymbol :: SymTable -> String -> Maybe SymbolEntry
lookupSymbol st n =
  let hits = [s | s@(SymbolEntry n' _ t _) <- st, n == n']
   in if hits /= [] then Just $ head hits else Nothing

lookupFieldByName :: FieldTable -> String -> Maybe FieldEntry
lookupFieldByName [] fn = Nothing
lookupFieldByName (fe@(FieldEntry n _ _) : ft) fn
  | fn == n = Just fe
  | otherwise = lookupFieldByName ft fn

lookupClosestMatchingProc :: ClassTable -> ProcTable -> String -> [Type] -> Either String ProcEntry
lookupClosestMatchingProc ct ps n ts = do
  -- First, calculate the procedures whose name and types are compatible
  let matchingProcs = filter (matchesNameAndType ct n ts) ps
  -- Second, calculate the minimum according to the partial order of type lists
  if null matchingProcs
    then Left $ "no matching procedure definition found for procedure " ++ n ++ " with actual parameter types " ++ show ts
    else -- if there is a matching procedure, get the lower bound according to the type partial order on lists
    case getTypeLowerBoundProc ct matchingProcs matchingProcs of
      Nothing -> Left $ "ambiguous type match for procedure " ++ n ++ " with actual parameter types " ++ show ts
      Just pe -> Right pe

-- The first string is the object name, the second is the method name
lookupClosestMatchingMethod :: ClassTable -> SymTable -> String -> String -> [Type] -> Either String (MethodID, ProcEntry)
lookupClosestMatchingMethod ct st o m ts = do
  -- First, calculate the methods whose name and types are compatible
  case lookupSymbol st o of
    Nothing -> Left $ "undefined symbol " ++ o
    Just (SymbolEntry _ _ INT _) -> Left $ "method call on VAR symbol " ++ o
    Just (SymbolEntry _ _ (OBJ cn) _) -> case lookupClassByName cn ct of
      Nothing -> Left $ "BUG encountered: undefined class for symbol " ++ o
      Just (_, ClassEntry cn _ _ mt) -> do
        let matchingMethods = filter (matchesNameAndType ct m ts) mt
        if null matchingMethods
          then Left $ "no matching method definition found for method " ++ m ++ " of class " ++ cn ++ " with actual parameter types " ++ show ts
          else -- if there are matching methods, get the lower bound according to the type partial order on lists
          case getTypeLowerBoundMethod ct matchingMethods matchingMethods of
            Nothing -> Left $ "ambiguous type match for method " ++ m ++ " of class " ++ cn ++ " with actual parameter types " ++ show ts
            Just me -> Right me
  where
    matchesNameAndType ct n ts (id, ProcEntry (Signature n' ts' _) _) = n == n' && areSubtypesOf ct ts ts'
    getTypeLowerBoundMethod ct [] ps = Nothing
    getTypeLowerBoundMethod ct (lp : lps) ps =
      if isTypeLowerBoundOf ct (getInputTypes . snd $ lp) (map (getInputTypes . snd) ps)
        then Just lp
        else getTypeLowerBoundMethod ct lps ps

matchesNameAndType :: ClassTable -> String -> [Type] -> ProcEntry -> Bool
matchesNameAndType ct n ts (ProcEntry (Signature n' ts' _) _) = n == n' && areSubtypesOf ct ts ts'

getTypeLowerBoundProc :: ClassTable -> [ProcEntry] -> [ProcEntry] -> Maybe ProcEntry
getTypeLowerBoundProc ct [] ps = Nothing
getTypeLowerBoundProc ct (lp : lps) ps =
  if isTypeLowerBoundOf ct (getInputTypes lp) (map getInputTypes ps)
    then Just lp
    else getTypeLowerBoundProc ct lps ps

getInputTypes :: ProcEntry -> [Type]
getInputTypes (ProcEntry (Signature _ ts _) _) = ts

getParamName :: FormalParameterDeclaration -> String
getParamName (VarParameter (Var n)) = n
getParamName (ObjectParameter (Object _ n)) = n

hasNameCollisions :: FormalParameterList -> Bool
hasNameCollisions [] = False
hasNameCollisions (p : fpl) = any (hasSameName p) fpl || hasNameCollisions fpl
  where
    hasSameName :: FormalParameterDeclaration -> FormalParameterDeclaration -> Bool
    hasSameName (VarParameter (Var n)) (VarParameter (Var n')) = n == n'
    hasSameName (ObjectParameter (Object _ n)) (ObjectParameter (Object _ n')) = n == n'
    hasSameName (VarParameter (Var n)) (ObjectParameter (Object _ n')) = n == n'
    hasSameName p p' = hasSameName p' p

isIntType :: OptionalType -> Bool
isIntType Nothing = False
isIntType (Just INT) = True
isIntType (Just (OBJ _)) = False

-- This function calculates the required memory a procedure needs to allocate for local variables declared in its code
calculateCommandStackMemoryRequirements :: SyntaxTree.Command -> Int
calculateCommandStackMemoryRequirements (ConstDeclarationCommand _) = 1
calculateCommandStackMemoryRequirements (VarDeclarationCommand _) = 1
calculateCommandStackMemoryRequirements (ObjectDeclarationCommand _) = 1
calculateCommandStackMemoryRequirements (Block (c :| [])) = calculateCommandStackMemoryRequirements c
{- If a block has at least 2 commands, the memory required is determined by the question if the first command is a block, too.
 - This is because of how blocks are compiled:
 - After compiling a block, its symbols get flushed from the table again because they should not be visible from outside
 - If the block is entered twice, the values are reset.
 - So if the (inner) block needs more space than the following commands, its memory requirement is dominant
 - Otherwise the requirement of the following commands is dominant - so we calculate the max value of both
 -}
calculateCommandStackMemoryRequirements (Block (c :| (c' : cs))) = case c of
  Block _ -> max (calculateCommandStackMemoryRequirements c) (calculateCommandStackMemoryRequirements $ Block $ c' :| cs)
  _ -> calculateCommandStackMemoryRequirements c + calculateCommandStackMemoryRequirements (Block $ c' :| cs)
calculateCommandStackMemoryRequirements (IfThen _ c) = calculateCommandStackMemoryRequirements c
calculateCommandStackMemoryRequirements (While _ c) = calculateCommandStackMemoryRequirements c
calculateCommandStackMemoryRequirements _ = 0

{--}

{- Class instances -}
{- Convention: Every generator ensures to clean up its state after itself -}
instance Generatable Program where
  generator (Program classes procs c) = do
    let numMethodTables = length classes
    -- There is 1 jump and additionally 1 method table creation command for every class at the start of the program
    prefixlength += 1 + numMethodTables
    classCommands <- traverse generator classes
    procCommands <- traverse (contextGenerator NORMAL) procs
    -- reserve stack memory for variable declarations in main program
    let mainStackMemoryAllocationCommands = replicate (calculateCommandStackMemoryRequirements c) (PushInt 0)
    prefixlength += length mainStackMemoryAllocationCommands
    mainProgram <- contextGenerator MAIN c
    ct <- use classtable
    let programCommands = [Reset] ++ getMTables ct ++ concat classCommands ++ concat procCommands ++ mainStackMemoryAllocationCommands ++ mainProgram ++ [PrintStr "\nProgram terminated successfully.\n", Halt]
    {- set state accordingly (not strictly necessary, but for sake of consistency with other generators, we do it none the less) -}
    prefixlength .= length programCommands
    symtable .= []
    proctable .= []
    classtable .= []
    {--}
    return programCommands
    where
      getMTables ct = map getMTable ct
      getMTable (id, ClassEntry _ _ _ mt) = CreateMethodTable id (map getMethodAddress mt)
      getMethodAddress (id, ProcEntry _ a) = (id, a)

instance Generatable ClassDeclaration where
  generator (Class n ps mc fields init methods) = do
    {- Generate template class table entry with inheritance information and add fields -}
    ct <- use classtable
    let newClassID = length ct
    -- With the template, we implement inheritance
    template <-
      ( case mc of
          -- IF there is not an upper class, create empty class template
          Nothing -> return $ ClassEntry n Nothing [] []
          -- IF there IS an upper class, we essentially copy its fields and methods into our template
          Just u -> case lookupClassByName u ct of
            Nothing -> throwE $ "invalid upper class " ++ u ++ " for class " ++ n
            (Just (uid, ClassEntry _ _ uft umt)) -> return $ ClassEntry n (Just uid) uft umt
        )
    -- Now we add the template into the class table and also add the fields
    classtable .= (newClassID, template) : ct
    traverse_ (addFieldToClassEntry newClassID) fields
    {- Generate initializer as procedure with object 'this' as implicit return parameter and preceding memory allocation -}
    let initProcedure = Procedure (ProcedureHeader ("INIT_" ++ n) ps (Just $ ObjectParameter $ Object n "this") []) init
    initCommands <- contextGenerator INIT initProcedure
    {- Generate methods -}
    methodCommands <- traverse (contextGenerator newClassID) methods
    return $ initCommands ++ concat methodCommands
    where
      addFieldToClassEntry :: ClassID -> FieldDeclaration -> GeneratorAction ()
      addFieldToClassEntry id (Field True _) = throwE "static fields unsupported"
      addFieldToClassEntry id (Field False pd) = do
        ct <- use classtable
        case lookup id ct of
          Nothing -> throwE "BUG encountered: trying to add field to non-existing class!"
          Just (ClassEntry cn ucc ft mt) -> case pd of
            VarParameter (Var n) -> classtable %= replaceClassInTable id (ClassEntry cn ucc (FieldEntry n INT (length ft) : ft) mt)
            ObjectParameter (Object t n) -> case lookupClassByName t ct of
              Nothing -> throwE $ "field " ++ n ++ " of class " ++ cn ++ " has invalid type " ++ t ++ "!"
              Just _ -> classtable %= replaceClassInTable id (ClassEntry cn ucc (FieldEntry n (OBJ t) (length ft) : ft) mt)

instance ContextGeneratable ClassID MethodDeclaration where
  contextGenerator id (Method (ProcedureHeader n pl mrp ps) c) = do
    {- Insert new entry / Override existing into method table of corresponding class -}
    -- Check for duplicate parameter names
    when (hasNameCollisions pl) $ throwE $ "parameter list of procedure " ++ n ++ " has duplicates"
    -- There will be one jump command before the actual method code starts
    prefixlength += 1
    methodCodeStart <- use prefixlength
    let newMethodEntry = ProcEntry (Signature n (map paramToType pl) (paramToType <$> mrp)) methodCodeStart
    updateClassTableWithNewMethod id newMethodEntry
    {- Generate sub-procedures -}
    -- Save the old procedure table for the reset later
    oldpt <- use proctable
    -- Generate the sub-procedure code
    subProcedureCommands <- traverse (contextGenerator NORMAL) ps
    {- Insert object, parameters and return parameter into symbol table -}
    ct <- use classtable
    thisParam <- case lookup id ct of
      Nothing -> throwE "BUG encountered: method has no corresponding class!"
      Just (ClassEntry n _ _ _) -> return $ ObjectParameter $ Object n "this"
    let params =
          thisParam : case mrp of
            Nothing -> pl
            Just rp -> if rp `elem` pl then pl else pl ++ [rp]
    st <- use symtable
    case addParamsToSymbols st params of
      Nothing -> throwE $ "One of " ++ n ++ "'s parameters shadows a const value"
      Just st' -> symtable .= st'
    stWithParams <- use symtable
    {- Generate method commands, including stack memory allocation -}
    stackMemoryAllocationCommands <- do
      let localVariableMemoryRequirements = calculateCommandStackMemoryRequirements c
      -- if return parameter is not in parameter list, allocate a stack cell for it, too
      let returnParameterMemoryRequirements =
            case mrp of
              Nothing -> 0
              Just rp -> if rp `elem` pl then 0 else 1
      let stackMemoryRequirements = localVariableMemoryRequirements + returnParameterMemoryRequirements
      prefixlength += stackMemoryRequirements
      return $ replicate stackMemoryRequirements (PushInt 0)
    methodCommands <- contextGenerator METHOD c
    {- Create necessary commands for return -}
    returnCommands <- case mrp of
      Nothing -> return [Return False]
      Just rp -> case lookupSymbol stWithParams (getParamName rp) of
        Nothing -> throwE "BUG encountered: return parameter missing from symbols!"
        Just (SymbolEntry _ _ _ p) -> return [LoadStack p, Return True]
    -- Update prefix
    prefixlength += length returnCommands
    {- Cleanup state -}
    -- Reset symbol table
    symtable .= []
    -- Cleanup subprocedures from procedure table
    proctable .= oldpt
    {- Return generated procedure code -}
    newPrefix <- use prefixlength
    return $ [Jump newPrefix] ++ concat subProcedureCommands ++ stackMemoryAllocationCommands ++ methodCommands ++ returnCommands
    where
      updateClassTableWithNewMethod :: ClassID -> ProcEntry -> GeneratorAction ()
      updateClassTableWithNewMethod id pe@(ProcEntry s _) = do
        ct <- use classtable
        case lookup id ct of
          Nothing -> throwE "BUG encountered: method has no corresponding class!"
          Just ce@(ClassEntry cn ucid ft mt) -> case lookupOverridableMethod ct s mt of
            -- If an overridable method doesn't exist yet, this method is new and can just be added with a new ID
            Nothing -> classtable .= replaceClassInTable id (ClassEntry cn ucid ft ((length mt, pe) : mt)) ct
            -- Otherwise, override the method from upper class with the same ID!
            Just (mid, ProcEntry _ _) -> classtable .= replaceClassInTable id (ClassEntry cn ucid ft (replaceMethodInTable mid pe mt)) ct

      replaceMethodInTable :: MethodID -> ProcEntry -> MethodTable -> MethodTable
      replaceMethodInTable id pe mt = (id, pe) : filter ((/=) id . fst) mt

      lookupOverridableMethod :: ClassTable -> Signature -> MethodTable -> Maybe (MethodID, ProcEntry)
      lookupOverridableMethod ct s [] = Nothing
      lookupOverridableMethod ct s ((id, p@(ProcEntry s' a)) : mt) =
        if overridesSig ct s s'
          then Just (id, p)
          else lookupOverridableMethod ct s mt
        where
          overridesSig ct (Signature n ts rt) (Signature n' ts' rt') = n == n' && ts == ts' && overridesRet ct rt rt'
          overridesRet _ Nothing Nothing = True
          overridesRet _ Nothing (Just t) = False
          overridesRet _ (Just t) Nothing = False
          overridesRet ct (Just t) (Just t') = isSubtypeOf ct t t'

instance ContextGeneratable ProcKind ProcedureDeclaration where
  contextGenerator kind (Procedure (ProcedureHeader n pl mrp ps) c) = do
    {- Insert new entry into procedure table -}
    -- Check for duplicate parameter names
    when (hasNameCollisions pl) $ throwE $ "parameter list of procedure " ++ n ++ " has duplicates"
    pt <- use proctable
    oldPrefix <- use prefixlength
    -- Address of new procedure must be the old prefix + 1, because of the jump command at the beginning that must be skipped
    let newProcEntry = ProcEntry (Signature n (map paramToType pl) (paramToType <$> mrp)) (oldPrefix + 1)
    -- Update state with new procedure entry
    proctable .= newProcEntry : pt
    {- Generate sub-procedures -}
    -- Increase prefix by 1 - we know there will be 1 new jump command for the upper procedure before the subprocedures
    prefixlength += 1
    -- Generate code for sub procedures
    subProcedureCommands <- traverse (contextGenerator NORMAL) ps
    {- Insert parameters and return parameter into symbol table -}
    let params = case mrp of
          Nothing -> pl
          Just rp -> if rp `elem` pl then pl else pl ++ [rp]
    st <- use symtable
    case addParamsToSymbols st params of
      Nothing -> throwE $ "One of " ++ n ++ "'s parameters shadows a const value"
      Just st' -> symtable .= st'
    stWithParams <- use symtable
    {- Generate procedure code, including stack memory allocation and, in case of initializers, heap memory allocation-}
    stackMemoryAllocationCommands <- do
      let localVariableMemoryRequirements = calculateCommandStackMemoryRequirements c
      -- if return parameter is not in parameter list, allocate a stack cell for it, too
      let returnParameterMemoryRequirements =
            case mrp of
              Nothing -> 0
              Just rp -> if rp `elem` pl then 0 else 1
      let stackMemoryRequirements = localVariableMemoryRequirements + returnParameterMemoryRequirements
      prefixlength += stackMemoryRequirements
      return $ replicate stackMemoryRequirements (PushInt 0)
    -- IF INIT procedure: generate memory allocation commands and update prefix accordingly
    heapMemoryAllocationCommands <- case kind of
      INIT -> do
        prefixlength += 2
        case mrp of
          Nothing -> throwE "BUG encountered: initializer without return value!"
          Just rp -> case lookupSymbol stWithParams (getParamName rp) of
            Nothing -> throwE "BUG encountered: return parameter missing from symbols!"
            Just (SymbolEntry _ _ t p) -> case t of
              INT -> throwE "BUG encountered: initializer with VAR return value!"
              OBJ s -> do
                ct <- use classtable
                case lookupClassByName s ct of
                  Nothing -> throwE "BUG encountered: initializer with invalid return type!"
                  Just (id, ClassEntry _ _ ft _) -> return [AllocateHeap (length ft) id, StoreStack p]
      _ -> return []
    -- generate the commands
    procedureCommands <- contextGenerator PROCEDURE c
    {- Create necesary commands for return -}
    returnCommands <- case mrp of
      Nothing -> return [Return False]
      Just rp -> case lookupSymbol stWithParams (getParamName rp) of
        Nothing -> throwE "BUG encountered: return parameter missing from symbols!"
        Just (SymbolEntry _ _ _ p) -> return [LoadStack p, Return True]
    -- Update prefix
    prefixlength += length returnCommands
    {- Cleanup state -}
    -- Reset symbol table
    symtable .= []
    -- Reset procedure table to only contain the topmost procedure, not the subprocedures
    proctable .= newProcEntry : pt
    {- Return generated procedure code -}
    newPrefix <- use prefixlength
    return $ [Jump newPrefix] ++ concat subProcedureCommands ++ stackMemoryAllocationCommands ++ heapMemoryAllocationCommands ++ procedureCommands ++ returnCommands

instance Generatable Call where
  generator (SymbolReference (NameReference n)) = do
    -- lookup name
    st <- use symtable
    pos <- case lookupSymbol st n of
      Nothing -> throwE $ "undefined symbol " ++ n
      Just (SymbolEntry _ _ _ p) -> return p
    -- the resulting command is just a load on the symbol's position
    prefixlength += 1
    return [LoadStack pos]
  generator (SymbolReference (FieldReference o f)) = do
    -- lookup object
    st <- use symtable
    (objType, objPos) <- case lookupSymbol st o of
      Nothing -> throwE $ "undefined symbol " ++ o
      Just (SymbolEntry _ _ INT _) -> throwE "trying to access field of a non-object"
      Just (SymbolEntry _ _ (OBJ t) p) -> return (t, p)
    ct <- use classtable
    -- lookup field
    fieldPos <- case lookupClassByName objType ct of
      Nothing -> throwE $ "BUG encountered: invalid class for object " ++ o
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft f of
        Nothing -> throwE $ "invalid field " ++ f ++ " reference for object " ++ o
        Just (FieldEntry _ _ p) -> return p
    -- the resulting commands are a stack load of the symbols position to push the object's address, then a heap load of the referenced field
    prefixlength += 2
    return [LoadStack objPos, LoadHeap fieldPos]
  generator (Call (NameReference n) apl) = do
    -- lookup procedure
    paramTypes <- traverse typify apl
    ct <- use classtable
    pt <- use proctable
    procAddress <- case sequenceA paramTypes of
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingProc ct pt n ts of
        Left e -> throwE e
        Right (ProcEntry _ a) -> return a
    -- generate parameter loading commands and procedure call
    paramCommands <- traverse generator apl
    prefixlength += 1 -- we only generate 1 additional command - the procedure call
    -- the result is all the commands to generate the parameter expressions in order followed by the procedure call
    return $ concat paramCommands ++ [CallProcedure procAddress (length apl)]
  generator (Call (FieldReference o m) apl) = do
    -- lookup method from method table
    paramTypes <- traverse typify apl
    ct <- use classtable
    st <- use symtable
    methodID <- case sequenceA paramTypes of
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingMethod ct st o m ts of
        Left e -> throwE e
        Right (id, ProcEntry _ _) -> return id
    -- generate parameter loading commands and method call
    -- first, lookup the object position
    objPos <- case lookupSymbol st o of
      Nothing -> throwE $ "call on invalid object symbol " ++ o
      Just (SymbolEntry _ _ _ p) -> return p
    let objectLoadingCommand = [LoadStack objPos]
    prefixlength += 1
    -- then generate the commands for all the actual parameters
    paramCommands <- traverse generator apl
    prefixlength += 1
    return $ objectLoadingCommand ++ concat paramCommands ++ [CallMethod methodID (length apl)]

instance Typeable Call where
  typifier (SymbolReference (NameReference n)) = do
    st <- view symtablet
    case lookupSymbol st n of
      Nothing -> throwE $ "undefined variable in expression: " ++ n
      -- an symbol's type is just the type declared in the symbol table
      Just (SymbolEntry _ _ t _) -> return $ Just t
  typifier (SymbolReference (FieldReference o f)) = do
    -- lookup object
    st <- view symtablet
    (objType, objPos) <- case lookupSymbol st o of
      Nothing -> throwE $ "undefined symbol " ++ o
      Just (SymbolEntry _ _ INT _) -> throwE "trying to access field of a non-object"
      Just (SymbolEntry _ _ (OBJ t) p) -> return (t, p)
    ct <- view classtablet
    -- lookup field
    case lookupClassByName objType ct of
      Nothing -> throwE $ "BUG encountered: invalid class for object " ++ o
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft f of
        Nothing -> throwE $ "invalid field " ++ f ++ " reference for object " ++ o
        -- a field reference's type is determined by the field type in the class declaration
        Just (FieldEntry _ t _) -> return $ Just t
  typifier (Call (NameReference n) apl) = do
    paramTypes <- traverse typifier apl
    pt <- view proctablet
    ct <- view classtablet
    case sequenceA paramTypes of
      -- IF one of the types is Nothing, there is a hole!
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingProc ct pt n ts of
        Left e -> throwE e
        -- the type of the procedure call is just the type of the closest matching procedure
        Right (ProcEntry (Signature _ _ rt) _) -> return rt
  typifier (Call (FieldReference o m) apl) = do
    paramTypes <- traverse typifier apl
    ct <- view classtablet
    st <- view symtablet
    case sequenceA paramTypes of
      Nothing -> throwE "type error: empty type in parameter hole!"
      Just ts -> case lookupClosestMatchingMethod ct st o m ts of
        Left e -> throwE e
        -- again, the lookup function does the magic here, we just return its result
        Right (_, ProcEntry (Signature _ _ rt) _) -> return rt

instance ContextGeneratable CommandContext SyntaxTree.Command where
  contextGenerator ctxt (Assignment (NameReference n) e) = do
    ct <- use classtable
    st <- use symtable
    (symType, symPos) <- case lookupSymbol st n of
      Nothing -> throwE $ "assignment to undefined variable " ++ n
      Just (SymbolEntry _ _ t p) -> return (t, p)
    mt <- typify e
    case mt of
      Nothing -> throwE $ "variable " ++ n ++ " was assigned an expression with empty type"
      Just ty ->
        if isSubtypeOf ct ty symType
          then do
            -- Type is correct - we can generate the commands
            eCommands <- generator e
            prefixlength += 1
            updateSymbolTableDependingOnCommandContext ctxt st
            return $ eCommands ++ [StoreStack symPos]
          else throwDiagnosticError $ "variable " ++ n ++ " was assigned an expression with incompatible type"
  contextGenerator ctxt (Assignment (FieldReference o f) e) = do
    -- lookup object
    st <- use symtable
    (objType, objPos) <- case lookupSymbol st o of
      Nothing -> throwE $ "undefined symbol " ++ o
      Just (SymbolEntry _ _ INT _) -> throwE "trying to access field of a non-object"
      Just (SymbolEntry _ _ (OBJ t) p) -> return (t, p)
    ct <- use classtable
    -- lookup field
    (fieldType, fieldPos) <- case lookupClassByName objType ct of
      Nothing -> throwE $ "BUG encountered: invalid class for object " ++ o
      Just (_, ClassEntry _ _ ft _) -> case lookupFieldByName ft f of
        Nothing -> throwE $ "invalid field " ++ f ++ " reference for object " ++ o
        Just (FieldEntry _ t p) -> return (t, p)
    -- check types
    exprType <- typify e
    case exprType of
      Nothing -> throwE "type error: trying to assign empty return value"
      Just ty ->
        if isSubtypeOf ct ty fieldType
          then return ()
          else throwE $ "type error: cannot assign to field, as type " ++ show ty ++ " of expression is not a subtype of field " ++ f ++ " with type " ++ show fieldType ++ " of object " ++ o
    -- generate commands
    let objAddrLoadCommand = [LoadStack objPos]
    prefixlength += 1
    expressionCommands <- generator e
    let storeCommands = [StoreHeap fieldPos]
    prefixlength += 1
    return $ objAddrLoadCommand ++ expressionCommands ++ storeCommands
  contextGenerator ctxt (ConstDeclarationCommand (SyntaxTree.Const name number)) = do
    -- Assemble new symbol entry and add it
    st <- use symtable
    st' <- case addSymbol st name True INT of
      Nothing -> throwE $ "const " ++ name ++ " shadows another const value"
      Just st' -> return st'
    symtable .= st'
    -- look up position of new symbol
    pos <- case lookupSymbol st' name of
      Nothing -> throwE "BUG encountered: impossibly, the symbol we just added vanished"
      Just (SymbolEntry _ _ _ p) -> return p
    -- return commands and update prefix as well symbol table
    prefixlength += 2
    -- possible optimization: if the context isn't INNER, the commands can be omitted altogether
    updateSymbolTableDependingOnCommandContext ctxt st
    return [PushInt number, StoreStack pos]
  contextGenerator ctxt (VarDeclarationCommand (Var n)) = do
    -- Assemble new symbol entry and add it
    st <- use symtable
    st' <- case addSymbol st n False INT of
      Nothing -> throwE $ "variable " ++ n ++ " shadows a const value"
      Just st' -> return st'
    symtable .= st'
    -- look up position of new symbol
    pos <- case lookupSymbol st' n of
      Nothing -> throwE "BUG encountered: impossibly, the symbol we just added vanished"
      Just (SymbolEntry _ _ _ p) -> return p
    -- return commands and update prefix as well symbol table
    prefixlength += 2
    -- possible optimization: if the context isn't INNER, the commands can be omitted altogether
    updateSymbolTableDependingOnCommandContext ctxt st
    return [PushInt 0, StoreStack pos]
  contextGenerator ctxt (ObjectDeclarationCommand (Object t n)) = do
    -- Check if class is valid
    ct <- use classtable
    case lookupClassByName t ct of
      Nothing -> throwE $ "invalid class " ++ t ++ " for object " ++ n
      Just _ -> return ()
    -- Assemble new symbol entry and add it
    st <- use symtable
    st' <- case addSymbol st n False (OBJ t) of
      Nothing -> throwE "BUG encountered: impossibly, the symbol we just added vanished"
      Just st' -> return st'
    symtable .= st'
    -- look up position of new symbol
    pos <- case lookupSymbol st' n of
      Nothing -> throwE "BUG encountered: impossibly, the symbol we just added vanished"
      Just (SymbolEntry _ _ _ p) -> return p
    -- return commands and update prefix as well as symbol table
    prefixlength += 2
    -- possible optimization: of the context isn't INNER, the commands can be omitted altogether
    updateSymbolTableDependingOnCommandContext ctxt st
    -- An object declaration doesn't allocate memory, the address will be invalid until the object is initialized
    return [PushInt (-1), StoreStack pos]
  contextGenerator ctxt (CallCommand call) = do
    t <- typify call
    case t of
      Nothing -> generator call
      Just _ -> throwE "type error: we can only fit empty return values here"
  contextGenerator ctxt (SyntaxTree.Read n) = do
    st <- use symtable
    pos <- case lookupSymbol st n of
      Nothing -> throwE $ "undefined symbol " ++ n
      Just (SymbolEntry _ _ (OBJ _) _) -> throwE $ "type error: " ++ n ++ " is an object, can't read an integer into it"
      Just (SymbolEntry _ _ INT p) -> return p
    prefixlength += 2
    return [Command.Read, StoreStack pos]
  contextGenerator _ (Block cs) = do
    -- save old symbol table for the reset after generating the commands - this implements scoping
    st <- use symtable
    cmds <- traverse (contextGenerator INNER) cs
    symtable .= st
    return $ concat cmds
  contextGenerator _ (IfThen cond cmd) = do
    st <- use symtable
    condCommands <- generator cond
    prefixlength += 1
    bodyCommands <- contextGenerator INNER cmd
    symtable .= st
    p <- use prefixlength
    return $ condCommands ++ [JumpIfFalse p] ++ bodyCommands
  contextGenerator ctxt (While cond cmd) = do
    st <- use symtable
    oldPrefix <- use prefixlength
    condCommands <- generator cond
    prefixlength += 1
    bodyCommands <- contextGenerator INNER cmd
    prefixlength += 1
    symtable .= st
    newPrefix <- use prefixlength
    return $ condCommands ++ [JumpIfFalse newPrefix] ++ bodyCommands ++ [Jump oldPrefix]
  contextGenerator ctxt (SyntaxTree.PrintI e) = do
    t <- typify e
    case t of
      Nothing -> throwE "type error: we can only fit integer values here"
      Just (OBJ _) -> throwE "type error: we can only fit integer values here"
      Just INT -> do
        eCmds <- generator e
        prefixlength += 1
        return $ eCmds ++ [PrintInt]
  contextGenerator ctxt (PrintS msg) = do
    prefixlength += 1
    return [PrintStr msg]
  contextGenerator ctxt (PrintLnS msg) = do
    prefixlength += 1
    return [PrintStrLn msg]
  contextGenerator ctxt Error = do
    prefixlength += 1
    return [Halt]

{- This action generator is for correctly setting the symbol table after a command is generated.
 - The correct behavior depends on the context:
 - Method-, Procedure- and Main-Program-Context: The symbol table should be reset
 - Inner command as part of a command block: The symbol table should NOT be reset
 - Following commands would need access to the new symbols that preceding commands in the same block generate, for example:
 -  WHILE 1 = 1 { VAR x
 -                x := 0 }
 - A block on the other hand will always reset the symbol table, to respect scoping rules
 - For example, the following should be illegal:
 - PROCEDURE foo(VAR x) { { VAR Y } Y := 0 }
 -}
updateSymbolTableDependingOnCommandContext :: CommandContext -> SymTable -> GeneratorAction ()
updateSymbolTableDependingOnCommandContext INNER _ = return ()
updateSymbolTableDependingOnCommandContext _ st = symtable .= st

instance Generatable Condition where
  generator (Comparison e r e') = do
    -- check types
    t <- typify e
    t' <- typify e'
    case t of
      Nothing -> throwE "type error: conditions can only be evaluated on integers"
      Just ty -> when (t /= t') $ throwE "type error: conditions can only be evaluated on integers"
    eCommands <- generator e
    e'Commands <- generator e'
    let newCmds = eCommands ++ e'Commands ++ [CombineBinary $ conv r]
    prefixlength += 1
    return newCmds
    where
      conv SyntaxTree.Equals = Command.Equals
      conv SyntaxTree.Smaller = Command.Smaller
      conv SyntaxTree.Greater = Command.Greater
  generator (Negation c) = do
    cmds <- generator c
    let newCmds = cmds ++ [CombineUnary Not]
    prefixlength += 1
    return newCmds

-- this generator expects the expression to be well-typed which needs to be ensured before calling
instance Generatable Expression where
  generator e@(Expression ((s, t) :| sts)) = do
    -- If there is a plus, we just generate the factor (works also if s has type OBJ _)
    -- If there is a minus, we generate PushInt 0 before and Command Minus after the factor, calculating its negated value
    firstFactorCommands <- case s of
      SyntaxTree.Plus -> generator t
      SyntaxTree.Minus -> do
        prefixlength += 1
        factorCommands <- generator t
        prefixlength += 1
        return $ [PushInt 0] ++ factorCommands ++ [CombineBinary Command.Minus]
    tsCommands <- traverse stGenerator sts
    return $ firstFactorCommands ++ concat tsCommands
    where
      stGenerator (s, t) = do
        tCommands <- generator t
        prefixlength += 1
        let signCommand = [CombineBinary $ conv s]
        return $ tCommands ++ signCommand
      conv SyntaxTree.Plus = Command.Plus
      conv SyntaxTree.Minus = Command.Minus

instance Typeable Expression where
  typifier (Expression ((s, t) :| ts)) = do
    ttype <- typifier t
    if null ts
      then return ttype
      else do
        -- we just typify all the terms - if there are multiple terms and one is not an integer, the type is invalid
        ttypes <- traverse (typifier . snd) ts
        if isIntType ttype && all isIntType ttypes
          then return $ Just INT
          else throwE "invalid type: non-integer value in algebraic term!"

instance Generatable Term where
  generator (Term f ofs) = do
    fCommands <- generator f
    ofsCommands <- traverse ofsGenerator ofs
    return $ fCommands ++ concat ofsCommands
    where
      ofsGenerator (o, f) = do
        fCommands <- generator f
        prefixlength += 1
        let oCommand = [CombineBinary $ conv o]
        return $ fCommands ++ oCommand
      conv SyntaxTree.Times = Command.Times
      conv SyntaxTree.Divide = Command.Divide

instance Typeable Term where
  typifier (Term f ofs) = do
    ftype <- typifier f
    if null ofs
      then return ftype
      else do
        -- we typify all factors - again, if there are multiple factors, one of them being of type OBJ _ will make the type invalid
        ftypes <- traverse (typifier . snd) ofs
        if isIntType ftype && all isIntType ftypes
          then return $ Just INT
          else throwE "invalid type: non-integer value in algebraic term!"

instance Generatable Factor where
  generator (CallFactor c) = generator c
  generator (ClassInstantiation cn apl) = generator (Call (NameReference $ "INIT_" ++ cn) apl)
  generator (Number n) = do
    prefixlength += 1
    return [PushInt n]
  generator (CompositeFactor e) = generator e

-- these don't introduce much new logic, just syntax tree decomposition for the most part
instance Typeable Factor where
  typifier (CallFactor c) = typifier c
  -- a class instantiation unsurprisingly has the same type as the corresponding initializer/constructor
  typifier (ClassInstantiation cn apl) = typifier (Call (NameReference $ "INIT_" ++ cn) apl)
  typifier (Number n) = return $ Just INT
  typifier (CompositeFactor e) = typifier e

{--}