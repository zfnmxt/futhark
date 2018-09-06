{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | This is an ever-changing syntax representation for Futhark.  Some
-- types, such as @Exp@, are parametrised by type and name
-- representation.  See the @https://futhark.readthedocs.org@ for a
-- language reference, or this module may be a little hard to
-- understand.
module Language.Futhark.Syntax
  (
   module Language.Futhark.Core

  -- * Types
  , Uniqueness(..)
  , IntType(..)
  , FloatType(..)
  , PrimType(..)
  , ArrayDim (..)
  , DimDecl (..)
  , ShapeDecl (..)
  , shapeRank
  , stripDims
  , unifyShapes
  , TypeName(..)
  , typeNameFromQualName
  , qualNameFromTypeName
  , TypeBase(..)
  , TypeArg(..)
  , TypeExp(..)
  , TypeArgExp(..)
  , RecordArrayElemTypeBase(..)
  , ArrayElemTypeBase(..)
  , CompType
  , PatternType
  , StructType
  , Diet(..)
  , TypeDeclBase (..)

    -- * Values
  , IntValue(..)
  , FloatValue(..)
  , PrimValue(..)
  , IsPrimValue(..)
  , Value(..)

  -- * Abstract syntax tree
  , BinOp (..)
  , IdentBase (..)
  , Inclusiveness(..)
  , DimIndexBase(..)
  , ExpBase(..)
  , FieldBase(..)
  , LoopFormBase (..)
  , PatternBase(..)
  , StreamForm(..)

  -- * Module language
  , SpecBase(..)
  , SigExpBase(..)
  , TypeRefBase(..)
  , SigBindBase(..)
  , ModExpBase(..)
  , ModBindBase(..)
  , ModParamBase(..)

  -- * Definitions
  , DocComment(..)
  , ValBindBase(..)
  , Liftedness(..)
  , TypeBindBase(..)
  , TypeParamBase(..)
  , typeParamName
  , ProgBase(..)
  , DecBase(..)

  -- * Miscellaneous
  , NoInfo(..)
  , Info(..)
  , Names
  , QualName(..)
  )
  where

import           Control.Applicative
import           Control.Monad
import           Data.Array
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Foldable
import           Data.Loc
import qualified Data.Map.Strict                  as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                         as S
import           Data.Traversable
import qualified Data.Semigroup as Sem
import           Prelude

import           Futhark.Representation.Primitive (FloatType (..),
                                                   FloatValue (..),
                                                   IntType (..), IntValue (..))
import           Futhark.Util.Pretty
import           Language.Futhark.Core

-- | Convenience class for deriving 'Show' instances for the AST.
class (Show vn,
       Show (f VName),
       Show (f Diet),
       Show (f String),
       Show (f [VName]),
       Show (f PatternType),
       Show (f CompType),
       Show (f (TypeBase () ())),
       Show (f Int),
       Show (f [TypeBase () ()]),
       Show (f StructType),
       Show (f (Names, StructType)),
       Show (f ([TypeBase () ()], PatternType)),
       Show (f (M.Map VName VName)),
       Show (f [RecordArrayElemTypeBase () Names]),
       Show (f Uniqueness),
       Show (f ([CompType], CompType))) => Showable f vn where

-- | No information functor.  Usually used for placeholder type- or
-- aliasing information.
data NoInfo a = NoInfo
              deriving (Eq, Ord, Show)

instance Show vn => Showable NoInfo vn where
instance Functor NoInfo where
  fmap _ NoInfo = NoInfo
instance Foldable NoInfo where
  foldr _ b NoInfo = b
instance Traversable NoInfo where
  traverse _ NoInfo = pure NoInfo

-- | Some information.  The dual to 'NoInfo'
newtype Info a = Info { unInfo :: a }
            deriving (Eq, Ord, Show)

instance Show vn => Showable Info vn where
instance Functor Info where
  fmap f (Info x) = Info $ f x
instance Foldable Info where
  foldr f b (Info x) = f x b
instance Traversable Info where
  traverse f (Info x) = Info <$> f x

-- | Low-level primitive types.
data PrimType = Signed IntType
              | Unsigned IntType
              | FloatType FloatType
              | Bool
              deriving (Eq, Ord, Show)

-- | Non-array values.
data PrimValue = SignedValue !IntValue
               | UnsignedValue !IntValue
               | FloatValue !FloatValue
               | BoolValue !Bool
               deriving (Eq, Ord, Show)

class IsPrimValue v where
  primValue :: v -> PrimValue

instance IsPrimValue Int where
  primValue = SignedValue . Int32Value . fromIntegral

instance IsPrimValue Int8 where
  primValue = SignedValue . Int8Value
instance IsPrimValue Int16 where
  primValue = SignedValue . Int16Value
instance IsPrimValue Int32 where
  primValue = SignedValue . Int32Value
instance IsPrimValue Int64 where
  primValue = SignedValue . Int64Value

instance IsPrimValue Word8 where
  primValue = UnsignedValue . Int8Value . fromIntegral
instance IsPrimValue Word16 where
  primValue = UnsignedValue . Int16Value . fromIntegral
instance IsPrimValue Word32 where
  primValue = UnsignedValue . Int32Value . fromIntegral
instance IsPrimValue Word64 where
  primValue = UnsignedValue . Int64Value . fromIntegral

instance IsPrimValue Float where
  primValue = FloatValue . Float32Value

instance IsPrimValue Double where
  primValue = FloatValue . Float64Value

instance IsPrimValue Bool where
  primValue = BoolValue

class (Eq dim, Ord dim) => ArrayDim dim where
  -- | @unifyDims x y@ combines @x@ and @y@ to contain their maximum
  -- common information, and fails if they conflict.
  unifyDims :: dim -> dim -> Maybe dim

instance ArrayDim () where
  unifyDims () () = Just ()

-- | Declaration of a dimension size.
data DimDecl vn = NamedDim (QualName vn)
                  -- ^ The size of the dimension is this name, which
                  -- must be in scope.  In a return type, this will
                  -- give rise to an assertion.
                | ConstDim Int
                  -- ^ The size is a constant.
                | AnyDim
                  -- ^ No dimension declaration.
                deriving (Eq, Ord, Show)

instance Functor DimDecl where
  fmap = fmapDefault

instance Foldable DimDecl where
  foldMap = foldMapDefault

instance Traversable DimDecl where
  traverse f (NamedDim qn) = NamedDim <$> traverse f qn
  traverse _ (ConstDim x) = pure $ ConstDim x
  traverse _ AnyDim = pure AnyDim

instance (Eq vn, Ord vn) => ArrayDim (DimDecl vn) where
  unifyDims AnyDim y = Just y
  unifyDims x AnyDim = Just x
  unifyDims (NamedDim x) (NamedDim y) | x == y = Just $ NamedDim x
  unifyDims (ConstDim x) (ConstDim y) | x == y = Just $ ConstDim x
  unifyDims _ _ = Nothing

-- | The size of an array type is a list of its dimension sizes.  If
-- 'Nothing', that dimension is of a (statically) unknown size.
newtype ShapeDecl dim = ShapeDecl { shapeDims :: [dim] }
                      deriving (Eq, Ord, Show)

instance Foldable ShapeDecl where
  foldr f x (ShapeDecl ds) = foldr f x ds

instance Traversable ShapeDecl where
  traverse f (ShapeDecl ds) = ShapeDecl <$> traverse f ds

instance Functor ShapeDecl where
  fmap f (ShapeDecl ds) = ShapeDecl $ map f ds

instance Sem.Semigroup (ShapeDecl dim) where
  ShapeDecl l1 <> ShapeDecl l2 = ShapeDecl $ l1 ++ l2

instance Monoid (ShapeDecl dim) where
  mempty = ShapeDecl []
  mappend = (Sem.<>)

-- | The number of dimensions contained in a shape.
shapeRank :: ShapeDecl dim -> Int
shapeRank = length . shapeDims

-- | @stripDims n shape@ strips the outer @n@ dimensions from
-- @shape@, returning 'Nothing' if this would result in zero or
-- fewer dimensions.
stripDims :: Int -> ShapeDecl dim -> Maybe (ShapeDecl dim)
stripDims i (ShapeDecl l)
  | i < length l = Just $ ShapeDecl $ drop i l
  | otherwise    = Nothing


-- | @unifyShapes x y@ combines @x@ and @y@ to contain their maximum
-- common information, and fails if they conflict.
unifyShapes :: ArrayDim dim => ShapeDecl dim -> ShapeDecl dim -> Maybe (ShapeDecl dim)
unifyShapes (ShapeDecl xs) (ShapeDecl ys) = do
  guard $ length xs == length ys
  ShapeDecl <$> zipWithM unifyDims xs ys

-- | A type name consists of qualifiers (for error messages) and a
-- 'VName' (for equality checking).
data TypeName = TypeName { typeQuals :: [VName], typeLeaf :: VName }
              deriving (Show)

instance Eq TypeName where
  TypeName _ x == TypeName _ y = x == y

instance Ord TypeName where
  TypeName _ x `compare` TypeName _ y = x `compare` y

typeNameFromQualName :: QualName VName -> TypeName
typeNameFromQualName (QualName qs x) = TypeName qs x

qualNameFromTypeName :: TypeName -> QualName VName
qualNameFromTypeName (TypeName qs x) = QualName qs x

-- | Types that can be elements of tuple-arrays.
data RecordArrayElemTypeBase dim as =
    RecordArrayElem (ArrayElemTypeBase dim as)
  | RecordArrayArrayElem (ArrayElemTypeBase dim as) (ShapeDecl dim) Uniqueness
  deriving (Eq, Show)

instance Bitraversable RecordArrayElemTypeBase where
  bitraverse f g (RecordArrayElem t) = RecordArrayElem <$> bitraverse f g t
  bitraverse f g (RecordArrayArrayElem a shape u) =
    RecordArrayArrayElem <$> bitraverse f g a <*> traverse f shape <*> pure u

instance Bifunctor RecordArrayElemTypeBase where
  bimap = bimapDefault

instance Bifoldable RecordArrayElemTypeBase where
  bifoldMap = bifoldMapDefault

data ArrayElemTypeBase dim as =
    ArrayPrimElem PrimType as
  | ArrayPolyElem TypeName [TypeArg dim as] as
  | ArrayRecordElem (M.Map Name (RecordArrayElemTypeBase dim as))
  deriving (Eq, Show)

instance Bitraversable ArrayElemTypeBase where
  bitraverse _ g (ArrayPrimElem t as) =
    ArrayPrimElem t <$> g as
  bitraverse f g (ArrayPolyElem t args as) =
    ArrayPolyElem t <$> traverse (bitraverse f g) args <*> g as
  bitraverse f g (ArrayRecordElem fs) =
    ArrayRecordElem <$> traverse (bitraverse f g) fs

instance Bifunctor ArrayElemTypeBase where
  bimap = bimapDefault

instance Bifoldable ArrayElemTypeBase where
  bifoldMap = bifoldMapDefault

-- | An expanded Futhark type is either an array, a prim type, a
-- tuple, or a type variable.  When comparing types for equality with
-- '==', aliases are ignored, but dimensions much match.  Function
-- parameter names are ignored.
data TypeBase dim as = Prim PrimType
                     | Array (ArrayElemTypeBase dim as) (ShapeDecl dim) Uniqueness
                     | Record (M.Map Name (TypeBase dim as))
                     | TypeVar as Uniqueness TypeName [TypeArg dim as]
                     | Arrow as (Maybe VName) (TypeBase dim as) (TypeBase dim as)
                     -- ^ The aliasing corresponds to the lexical
                     -- closure of the function.
                     deriving (Show)

instance (Eq dim, Eq as) => Eq (TypeBase dim as) where
  Prim x1 == Prim y1 = x1 == y1
  Array x1 y1 z1 == Array x2 y2 z2 = x1 == x2 && y1 == y2 && z1 == z2
  Record x1 == Record x2 = x1 == x2
  TypeVar _ u1 x1 y1 == TypeVar _ u2 x2 y2 = u1 == u2 && x1 == x2 && y1 == y2
  Arrow _ _ x1 y1 == Arrow _ _ x2 y2 = x1 == x2 && y1 == y2
  _ == _ = False

instance Bitraversable TypeBase where
  bitraverse _ _ (Prim t) = pure $ Prim t
  bitraverse f g (Array a shape u) =
    Array <$> bitraverse f g a <*> traverse f shape <*> pure u
  bitraverse f g (Record fs) = Record <$> traverse (bitraverse f g) fs
  bitraverse f g (TypeVar als u t args) =
    TypeVar <$> g als <*> pure u <*> pure t <*> traverse (bitraverse f g) args
  bitraverse f g (Arrow als v t1 t2) =
    Arrow <$> g als <*> pure v <*> bitraverse f g t1 <*> bitraverse f g t2

instance Bifunctor TypeBase where
  bimap = bimapDefault

instance Bifoldable TypeBase where
  bifoldMap = bifoldMapDefault

data TypeArg dim as = TypeArgDim dim SrcLoc
                    | TypeArgType (TypeBase dim as) SrcLoc
             deriving (Eq, Show)

instance Bitraversable TypeArg where
  bitraverse f _ (TypeArgDim v loc) = TypeArgDim <$> f v <*> pure loc
  bitraverse f g (TypeArgType t loc) = TypeArgType <$> bitraverse f g t <*> pure loc

instance Bifunctor TypeArg where
  bimap = bimapDefault

instance Bifoldable TypeArg where
  bifoldMap = bifoldMapDefault

-- | A type with aliasing information and no shape annotations, used
-- for describing the type of a computation.
type CompType = TypeBase () Names

-- | A type with aliasing information and shape annotations, used for
-- describing the type of a pattern.
type PatternType = TypeBase (DimDecl VName) Names

-- | An unstructured type with type variables and possibly shape
-- declarations - this is what the user types in the source program.
data TypeExp vn = TEVar (QualName vn) SrcLoc
                | TETuple [TypeExp vn] SrcLoc
                | TERecord [(Name, TypeExp vn)] SrcLoc
                | TEArray (TypeExp vn) (DimDecl vn) SrcLoc
                | TEUnique (TypeExp vn) SrcLoc
                | TEApply (TypeExp vn) (TypeArgExp vn) SrcLoc
                | TEArrow (Maybe vn) (TypeExp vn) (TypeExp vn) SrcLoc
                | TESum [(Name, [TypeExp vn])] SrcLoc
                 deriving (Eq, Show)

instance Located (TypeExp vn) where
  locOf (TEArray _ _ loc)   = locOf loc
  locOf (TETuple _ loc)     = locOf loc
  locOf (TERecord _ loc)    = locOf loc
  locOf (TEVar _ loc)       = locOf loc
  locOf (TEUnique _ loc)    = locOf loc
  locOf (TEApply _ _ loc)   = locOf loc
  locOf (TEArrow _ _ _ loc) = locOf loc
  locOf (TESum _ loc) = locOf loc

data TypeArgExp vn = TypeArgExpDim (DimDecl vn) SrcLoc
                   | TypeArgExpType (TypeExp vn)
                deriving (Eq, Show)

instance Located (TypeArgExp vn) where
  locOf (TypeArgExpDim _ loc) = locOf loc
  locOf (TypeArgExpType t)    = locOf t

-- | A "structural" type with shape annotations and no aliasing
-- information, used for declarations.
type StructType = TypeBase (DimDecl VName) ()

-- | A declaration of the type of something.
data TypeDeclBase f vn =
  TypeDecl { declaredType :: TypeExp vn
                             -- ^ The type declared by the user.
           , expandedType :: f StructType
                             -- ^ The type deduced by the type checker.
           }
deriving instance Showable f vn => Show (TypeDeclBase f vn)

instance Located (TypeDeclBase f vn) where
  locOf = locOf . declaredType

-- | Information about which parts of a value/type are consumed.
data Diet = RecordDiet (M.Map Name Diet) -- ^ Consumes these fields in the record.
          | FuncDiet Diet Diet
            -- ^ A function that consumes its argument(s) like this.
            -- The final 'Diet' should always be 'Observe', as there
            -- is no way for a function to consume its return value.
          | Consume -- ^ Consumes this value.
          | Observe -- ^ Only observes value in this position, does
                    -- not consume.
            deriving (Eq, Show)

-- | Simple Futhark values.  Values are fully evaluated and their type
-- is always unambiguous.
data Value = PrimValue !PrimValue
           | ArrayValue !(Array Int Value) (TypeBase () ())
             -- ^ It is assumed that the array is 0-indexed.  The type
             -- is the full type.
             deriving (Eq, Show)

-- | An identifier consists of its name and the type of the value
-- bound to the identifier.
data IdentBase f vn = Ident { identName   :: vn
                            , identType   :: f CompType
                            , identSrcLoc :: SrcLoc
                            }
deriving instance Showable f vn => Show (IdentBase f vn)

instance Eq vn => Eq (IdentBase ty vn) where
  x == y = identName x == identName y

instance Ord vn => Ord (IdentBase ty vn) where
  compare = comparing identName

instance Located (IdentBase ty vn) where
  locOf = locOf . identSrcLoc

-- | Default binary operators.
data BinOp =  Backtick
              -- ^ A pseudo-operator standing in for any normal
              -- identifier used as an operator (they all have the
              -- same fixity).
           -- Binary Ops for Numbers
           | Plus
           | Minus
           | Pow
           | Times
           | Divide
           | Mod
           | Quot
           | Rem
           | ShiftR
           | ZShiftR -- ^ Zero-extend right shift.
           | ShiftL
           | Band
           | Xor
           | Bor
           | LogAnd
           | LogOr
           -- Relational Ops for all primitive types at least
           | Equal
           | NotEqual
           | Less
           | Leq
           | Greater
           | Geq
           -- Some functional ops.
           | PipeRight -- ^ @|>@
           | PipeLeft -- ^ @<|@
           -- Misc
             deriving (Eq, Ord, Show, Enum, Bounded)

-- | Whether a bound for an end-point of a 'DimSlice' or a range
-- literal is inclusive or exclusive.
data Inclusiveness a = DownToExclusive a
                     | ToInclusive a -- ^ May be "down to" if step is negative.
                     | UpToExclusive a
                     deriving (Eq, Ord, Show)

instance Located a => Located (Inclusiveness a) where
  locOf (DownToExclusive x) = locOf x
  locOf (ToInclusive x) = locOf x
  locOf (UpToExclusive x) = locOf x

instance Functor Inclusiveness where
  fmap = fmapDefault

instance Foldable Inclusiveness where
  foldMap = foldMapDefault

instance Traversable Inclusiveness where
  traverse f (DownToExclusive x) = DownToExclusive <$> f x
  traverse f (ToInclusive x) = ToInclusive <$> f x
  traverse f (UpToExclusive x) = UpToExclusive <$> f x

-- | An indexing of a single dimension.
data DimIndexBase f vn = DimFix (ExpBase f vn)
                       | DimSlice (Maybe (ExpBase f vn))
                                  (Maybe (ExpBase f vn))
                                  (Maybe (ExpBase f vn))
deriving instance Showable f vn => Show (DimIndexBase f vn)

-- | A name qualified with a breadcrumb of module accesses.
data QualName vn = QualName { qualQuals :: ![vn]
                            , qualLeaf  :: !vn
                            }
  deriving (Eq, Ord, Show)

instance Functor QualName where
  fmap = fmapDefault

instance Foldable QualName where
  foldMap = foldMapDefault

instance Traversable QualName where
  traverse f (QualName qs v) = QualName <$> traverse f qs <*> f v

-- | The Futhark expression language.
--
-- In a value of type @Exp f vn@, annotations are wrapped in the
-- functor @f@, and all names are of type @vn@.
--
-- This allows us to encode whether or not the expression has been
-- type-checked in the Haskell type of the expression.  Specifically,
-- the parser will produce expressions of type @Exp 'NoInfo' 'Name'@,
-- and the type checker will convert these to @Exp 'Info' 'VName'@, in
-- which type information is always present and all names are unique.
data ExpBase f vn =
              Literal PrimValue SrcLoc

            | IntLit Integer (f (TypeBase () ())) SrcLoc
            -- ^ A polymorphic integral literal.

            | FloatLit Double (f (TypeBase () ())) SrcLoc
            -- ^ A polymorphic decimal literal.

            | Parens (ExpBase f vn) SrcLoc
            -- ^ A parenthesized expression.

            | QualParens (QualName vn) (ExpBase f vn) SrcLoc

            | TupLit    [ExpBase f vn] SrcLoc
            -- ^ Tuple literals, e.g., @{1+3, {x, y+z}}@.

            | RecordLit [FieldBase f vn] SrcLoc
            -- ^ Record literals, e.g. @{x=2,y=3,z}@.

            | ArrayLit  [ExpBase f vn] (f CompType) SrcLoc
            -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
            -- Second arg is the row type of the rows of the array.

            | Range (ExpBase f vn) (Maybe (ExpBase f vn)) (Inclusiveness (ExpBase f vn)) (f CompType) SrcLoc

            | Var (QualName vn) (f PatternType) SrcLoc

            | Ascript (ExpBase f vn) (TypeDeclBase f vn) SrcLoc
            -- ^ Type ascription: @e : t@.

            | LetPat [TypeParamBase vn] (PatternBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc

            | LetFun vn ([TypeParamBase vn], [PatternBase f vn], Maybe (TypeExp vn), f StructType, ExpBase f vn)
              (ExpBase f vn) SrcLoc

            | If     (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) (f CompType) SrcLoc

            | Apply (ExpBase f vn) (ExpBase f vn) (f Diet) (f PatternType) SrcLoc

            | Negate (ExpBase f vn) SrcLoc
              -- ^ Numeric negation (ugly special case; Haskell did it first).

            | Lambda [TypeParamBase vn] [PatternBase f vn] (ExpBase f vn)
              (Maybe (TypeDeclBase f vn)) (f (Names, StructType)) SrcLoc

            | OpSection (QualName vn) (f PatternType) SrcLoc
              -- ^ @+@; first two types are operands, third is result.
            | OpSectionLeft (QualName vn) (f PatternType)
              (ExpBase f vn) (f StructType, f StructType) (f PatternType) SrcLoc
              -- ^ @2+@; first type is operand, second is result.
            | OpSectionRight (QualName vn) (f PatternType)
              (ExpBase f vn) (f StructType, f StructType) (f PatternType) SrcLoc
              -- ^ @+2@; first type is operand, second is result.
            | ProjectSection [Name] (f PatternType) SrcLoc
              -- ^ Field projection as a section: @(.x.y.z)@.
            | IndexSection [DimIndexBase f vn] (f PatternType) SrcLoc
              -- ^ Array indexing as a section: @(.[i,j])@.

            | DoLoop
              [TypeParamBase vn]
              (PatternBase f vn) -- Merge variable pattern
              (ExpBase f vn) -- Initial values of merge variables.
              (LoopFormBase f vn) -- Do or while loop.
              (ExpBase f vn) -- Loop body.
              SrcLoc

            | BinOp (QualName vn) (f PatternType)
              (ExpBase f vn, f StructType) (ExpBase f vn, f StructType)
              (f PatternType) SrcLoc
            -- ^ The first annotation is the instantiation list.

            | Project Name (ExpBase f vn) (f CompType) SrcLoc

            -- Primitive array operations
            | LetWith (IdentBase f vn) (IdentBase f vn)
                      [DimIndexBase f vn] (ExpBase f vn)
                      (ExpBase f vn) SrcLoc

            | Index (ExpBase f vn) [DimIndexBase f vn] (f CompType) SrcLoc

            | Update (ExpBase f vn) [DimIndexBase f vn] (ExpBase f vn) SrcLoc

            -- Second-Order Array Combinators accept curried and
            -- anonymous functions as first params.
            | Map (ExpBase f vn) (ExpBase f vn) (f CompType) SrcLoc
             -- ^ @map (+1) [1, 2, ..., n] = [2, 3, ..., n+1]@.

            | Reduce Commutativity (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc
             -- ^ @reduce (+) 0 ([1,2,...,n]) = (0+1+2+...+n)@.

            | Scan (ExpBase f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc
             -- ^ @scan (+) 0 ([ 1, 2, 3 ]) = [ 1, 3, 6 ]@.

            | Filter (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ Return those elements of the array that satisfy the
            -- predicate.

            | Partition Int (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ @partition k f a@, where @f@ returns an integer,
            -- returns a tuple @(a', is)@ that describes a
            -- partitioning of @a@ into @n@ equivalence classes.
            -- Here, @a'@ is a re-ordering of @a@, and @is@ is an
            -- array of @k@ offsets into @a'@.

            | Stream (StreamForm f vn) (ExpBase f vn) (ExpBase f vn) SrcLoc
            -- ^ Streaming: intuitively, this gives a size-parameterized
            -- composition for SOACs that cannot be fused, e.g., due to scan.
            -- For example, assuming @A : [int], f : int->int, g : real->real@,
            -- the code: @let x = map(f,A) in let y = scan(op+,0,x) in map(g,y)@
            -- can be re-written (streamed) in the source-Futhark language as:
            -- @let (acc, z) =@
            -- @  stream (fn (int,[real]) (real chunk, real acc, [int] a) =>@
            -- @            let x = map (f,         A )@
            -- @            let y0= scan(op +, 0,   x )@
            -- @            let y = map (op +(acc), y0)@
            -- @            ( acc+y0[chunk-1], map(g, y) )@
            -- @         ) 0 A@
            -- where (i)  @chunk@ is a symbolic int denoting the chunk
            -- size, (ii) @0@ is the initial value of the accumulator,
            -- which allows the streaming of @scan@.
            -- Finally, the unnamed function (@fn...@) implements the a fold that:
            -- computes the accumulator of @scan@, as defined inside its body, AND
            -- implicitly concatenates each of the result arrays across
            -- the iteration space.
            -- In essence, sequential codegen can choose chunk = 1 and thus
            -- eliminate the SOACs on the outermost level, while parallel codegen
            -- may choose the maximal chunk size that still satisfies the memory
            -- requirements of the device.

            | Zip Int (ExpBase f vn) [ExpBase f vn] (f CompType) SrcLoc
            -- ^ Conventional zip taking nonzero arrays as arguments.
            -- All arrays must have the exact same length.

            | Unzip (ExpBase f vn) [f CompType] SrcLoc
            -- ^ Unzip that can unzip to tuples of arbitrary size.
            -- The types are the elements of the tuple.

            | Unsafe (ExpBase f vn) SrcLoc
            -- ^ Explore the Danger Zone and elide safety checks on
            -- array operations and other assertions during execution
            -- of this expression.  Make really sure the code is
            -- correct.

            | Assert (ExpBase f vn) (ExpBase f vn) (f String) SrcLoc
            -- ^ Fail if the first expression does not return true,
            -- and return the value of the second expression if it
            -- does.

deriving instance Showable f vn => Show (ExpBase f vn)

data StreamForm f vn = MapLike    StreamOrd
                     | RedLike    StreamOrd Commutativity (ExpBase f vn)
deriving instance Showable f vn => Show (StreamForm f vn)

instance Located (ExpBase f vn) where
  locOf (Literal _ loc)                = locOf loc
  locOf (IntLit _ _ loc)               = locOf loc
  locOf (FloatLit _ _ loc)             = locOf loc
  locOf (Parens _ loc)                 = locOf loc
  locOf (QualParens _ _ loc)           = locOf loc
  locOf (TupLit _ pos)                 = locOf pos
  locOf (RecordLit _ pos)              = locOf pos
  locOf (Project _ _ _ pos)            = locOf pos
  locOf (ArrayLit _ _ pos)             = locOf pos
  locOf (Range _ _ _ _ pos)            = locOf pos
  locOf (BinOp _ _ _ _ _ pos)          = locOf pos
  locOf (If _ _ _ _ pos)               = locOf pos
  locOf (Var _ _ loc)                  = locOf loc
  locOf (Ascript _ _ loc)              = locOf loc
  locOf (Negate _ pos)                 = locOf pos
  locOf (Apply _ _ _ _ pos)            = locOf pos
  locOf (LetPat _ _ _ _ pos)           = locOf pos
  locOf (LetFun _ _ _ loc)             = locOf loc
  locOf (LetWith _ _ _ _ _ pos)        = locOf pos
  locOf (Index _ _ _ loc)              = locOf loc
  locOf (Update _ _ _ pos)             = locOf pos
  locOf (Map _ _ _ loc)                = locOf loc
  locOf (Reduce _ _ _ _ pos)           = locOf pos
  locOf (Zip _ _ _ _ loc)              = locOf loc
  locOf (Unzip _ _ pos)                = locOf pos
  locOf (Scan _ _ _ pos)               = locOf pos
  locOf (Filter _ _ pos)               = locOf pos
  locOf (Partition _ _ _ loc)          = locOf loc
  locOf (Lambda _ _ _ _ _ loc)         = locOf loc
  locOf (OpSection _ _ loc)            = locOf loc
  locOf (OpSectionLeft _ _ _ _ _ loc)  = locOf loc
  locOf (OpSectionRight _ _ _ _ _ loc) = locOf loc
  locOf (ProjectSection _ _ loc)       = locOf loc
  locOf (IndexSection _ _ loc)         = locOf loc
  locOf (DoLoop _ _ _ _ _ pos)         = locOf pos
  locOf (Stream _ _ _  pos)            = locOf pos
  locOf (Unsafe _ loc)                 = locOf loc
  locOf (Assert _ _ _ loc)             = locOf loc

-- | An entry in a record literal.
data FieldBase f vn = RecordFieldExplicit Name (ExpBase f vn) SrcLoc
                    | RecordFieldImplicit vn (f CompType) SrcLoc

deriving instance Showable f vn => Show (FieldBase f vn)

instance Located (FieldBase f vn) where
  locOf (RecordFieldExplicit _ _ loc) = locOf loc
  locOf (RecordFieldImplicit _ _ loc) = locOf loc

-- | Whether the loop is a @for@-loop or a @while@-loop.
data LoopFormBase f vn = For (IdentBase f vn) (ExpBase f vn)
                       | ForIn (PatternBase f vn) (ExpBase f vn)
                       | While (ExpBase f vn)
deriving instance Showable f vn => Show (LoopFormBase f vn)

-- | A pattern as used most places where variables are bound (function
-- parameters, @let@ expressions, etc).
data PatternBase f vn = TuplePattern [PatternBase f vn] SrcLoc
                      | RecordPattern [(Name, PatternBase f vn)] SrcLoc
                      | PatternParens (PatternBase f vn) SrcLoc
                      | Id vn (f PatternType) SrcLoc
                      | Wildcard (f PatternType) SrcLoc -- Nothing, i.e. underscore.
                      | PatternAscription (PatternBase f vn) (TypeDeclBase f vn) SrcLoc
deriving instance Showable f vn => Show (PatternBase f vn)

instance Located (PatternBase f vn) where
  locOf (TuplePattern _ loc)        = locOf loc
  locOf (RecordPattern _ loc)       = locOf loc
  locOf (PatternParens _ loc)       = locOf loc
  locOf (Id _ _ loc)                = locOf loc
  locOf (Wildcard _ loc)            = locOf loc
  locOf (PatternAscription _ _ loc) = locOf loc

-- | Documentation strings, including source location.
data DocComment = DocComment String SrcLoc
  deriving (Show)

instance Located DocComment where
  locOf (DocComment _ loc) = locOf loc

-- | Function Declarations
data ValBindBase f vn = ValBind { valBindEntryPoint :: Bool
                                -- ^ True if this function is an entry point.
                                , valBindName       :: vn
                                , valBindRetDecl    :: Maybe (TypeExp vn)
                                , valBindRetType    :: f StructType
                                , valBindTypeParams :: [TypeParamBase vn]
                                , valBindParams     :: [PatternBase f vn]
                                , valBindBody       :: ExpBase f vn
                                , valBindDoc        :: Maybe DocComment
                                , valBindLocation   :: SrcLoc
                                }
deriving instance Showable f vn => Show (ValBindBase f vn)

instance Located (ValBindBase f vn) where
  locOf = locOf . valBindLocation

-- | Type Declarations
data TypeBindBase f vn = TypeBind { typeAlias        :: vn
                                  , typeParams       :: [TypeParamBase vn]
                                  , typeExp          :: TypeDeclBase f vn
                                  , typeDoc          :: Maybe DocComment
                                  , typeBindLocation :: SrcLoc
                                  }
deriving instance Showable f vn => Show (TypeBindBase f vn)

instance Located (TypeBindBase f vn) where
  locOf = locOf . typeBindLocation

-- | The liftedness of a type parameter.  By the @Ord@ instance,
-- @Unlifted@ is less than @Lifted@.
data Liftedness = Unlifted -- ^ May only be instantiated with a zero-order type.
                | Lifted -- ^ May be instantiated to a functional type.
                deriving (Eq, Ord, Show)

data TypeParamBase vn = TypeParamDim vn SrcLoc
                        -- ^ A type parameter that must be a size.
                      | TypeParamType Liftedness vn SrcLoc
                        -- ^ A type parameter that must be a type.
  deriving (Eq, Show)

instance Functor TypeParamBase where
  fmap = fmapDefault

instance Foldable TypeParamBase where
  foldMap = foldMapDefault

instance Traversable TypeParamBase where
  traverse f (TypeParamDim v loc) = TypeParamDim <$> f v <*> pure loc
  traverse f (TypeParamType l v loc) = TypeParamType l <$> f v <*> pure loc

instance Located (TypeParamBase vn) where
  locOf (TypeParamDim _ loc)    = locOf loc
  locOf (TypeParamType _ _ loc) = locOf loc

typeParamName :: TypeParamBase vn -> vn
typeParamName (TypeParamDim v _)    = v
typeParamName (TypeParamType _ v _) = v

data SpecBase f vn = ValSpec  { specName       :: vn
                              , specTypeParams :: [TypeParamBase vn]
                              , specType       :: TypeDeclBase f vn
                              , specDoc        :: Maybe DocComment
                              , specLocation   :: SrcLoc
                              }
                   | TypeAbbrSpec (TypeBindBase f vn)
                   | TypeSpec Liftedness vn [TypeParamBase vn] (Maybe DocComment) SrcLoc -- ^ Abstract type.
                   | ModSpec vn (SigExpBase f vn) (Maybe DocComment) SrcLoc
                   | IncludeSpec (SigExpBase f vn) SrcLoc
deriving instance Showable f vn => Show (SpecBase f vn)

instance Located (SpecBase f vn) where
  locOf (ValSpec _ _ _ _ loc)  = locOf loc
  locOf (TypeAbbrSpec tbind)   = locOf tbind
  locOf (TypeSpec _ _ _ _ loc) = locOf loc
  locOf (ModSpec _ _ _ loc)    = locOf loc
  locOf (IncludeSpec _ loc)    = locOf loc

data SigExpBase f vn = SigVar (QualName vn) SrcLoc
                     | SigParens (SigExpBase f vn) SrcLoc
                     | SigSpecs [SpecBase f vn] SrcLoc
                     | SigWith (SigExpBase f vn) (TypeRefBase f vn) SrcLoc
                     | SigArrow (Maybe vn) (SigExpBase f vn) (SigExpBase f vn) SrcLoc
deriving instance Showable f vn => Show (SigExpBase f vn)

-- | A type refinement.
data TypeRefBase f vn = TypeRef (QualName vn) [TypeParamBase vn] (TypeDeclBase f vn) SrcLoc
deriving instance Showable f vn => Show (TypeRefBase f vn)

instance Located (TypeRefBase f vn) where
  locOf (TypeRef _ _ _ loc) = locOf loc

instance Located (SigExpBase f vn) where
  locOf (SigVar _ loc)       = locOf loc
  locOf (SigParens _ loc)    = locOf loc
  locOf (SigSpecs _ loc)     = locOf loc
  locOf (SigWith _ _ loc)    = locOf loc
  locOf (SigArrow _ _ _ loc) = locOf loc

data SigBindBase f vn = SigBind { sigName :: vn
                                , sigExp  :: SigExpBase f vn
                                , sigDoc  :: Maybe DocComment
                                , sigLoc  :: SrcLoc
                                }
deriving instance Showable f vn => Show (SigBindBase f vn)

instance Located (SigBindBase f vn) where
  locOf = locOf . sigLoc

data ModExpBase f vn = ModVar (QualName vn) SrcLoc
                     | ModParens (ModExpBase f vn) SrcLoc
                     | ModImport FilePath (f FilePath) SrcLoc
                       -- ^ The contents of another file as a module.
                     | ModDecs [DecBase f vn] SrcLoc
                     | ModApply (ModExpBase f vn) (ModExpBase f vn) (f (M.Map VName VName)) (f (M.Map VName VName)) SrcLoc
                       -- ^ Functor application.
                     | ModAscript (ModExpBase f vn) (SigExpBase f vn) (f (M.Map VName VName)) SrcLoc
                     | ModLambda (ModParamBase f vn)
                                 (Maybe (SigExpBase f vn, f (M.Map VName VName)))
                                 (ModExpBase f vn)
                                 SrcLoc
deriving instance Showable f vn => Show (ModExpBase f vn)

instance Located (ModExpBase f vn) where
  locOf (ModVar _ loc)         = locOf loc
  locOf (ModParens _ loc)      = locOf loc
  locOf (ModImport _ _ loc)    = locOf loc
  locOf (ModDecs _ loc)        = locOf loc
  locOf (ModApply _ _ _ _ loc) = locOf loc
  locOf (ModAscript _ _ _ loc) = locOf loc
  locOf (ModLambda _ _ _ loc)  = locOf loc

data ModBindBase f vn =
  ModBind { modName      :: vn
          , modParams    :: [ModParamBase f vn]
          , modSignature :: Maybe (SigExpBase f vn, f (M.Map VName VName))
          , modExp       :: ModExpBase f vn
          , modDoc       :: Maybe DocComment
          , modLocation  :: SrcLoc
          }
deriving instance Showable f vn => Show (ModBindBase f vn)

instance Located (ModBindBase f vn) where
  locOf = locOf . modLocation

data ModParamBase f vn = ModParam { modParamName     :: vn
                                  , modParamType     :: SigExpBase f vn
                                  , modParamAbs      :: f [VName]
                                  , modParamLocation :: SrcLoc
                                  }
deriving instance Showable f vn => Show (ModParamBase f vn)

instance Located (ModParamBase f vn) where
  locOf = locOf . modParamLocation

-- | A top-level binding.
data DecBase f vn = ValDec (ValBindBase f vn)
                  | TypeDec (TypeBindBase f vn)
                  | SigDec (SigBindBase f vn)
                  | ModDec (ModBindBase f vn)
                  | OpenDec (ModExpBase f vn) [ModExpBase f vn] (f [VName]) SrcLoc
                  | LocalDec (DecBase f vn) SrcLoc
deriving instance Showable f vn => Show (DecBase f vn)

instance Located (DecBase f vn) where
  locOf (ValDec d)          = locOf d
  locOf (TypeDec d)         = locOf d
  locOf (SigDec d)          = locOf d
  locOf (ModDec d)          = locOf d
  locOf (OpenDec _ _ _ loc) = locOf loc
  locOf (LocalDec _ loc)    = locOf loc

-- | The program described by a single Futhark file.  May depend on
-- other files.
data ProgBase f vn = Prog { progDoc :: Maybe DocComment
                          , progDecs :: [DecBase f vn]
                          }
deriving instance Showable f vn => Show (ProgBase f vn)

-- | A set of names.
type Names = S.Set VName

--- Some prettyprinting definitions are here because we need them in
--- the Attributes module.

instance Pretty PrimType where
  ppr (Unsigned Int8)  = text "u8"
  ppr (Unsigned Int16) = text "u16"
  ppr (Unsigned Int32) = text "u32"
  ppr (Unsigned Int64) = text "u64"
  ppr (Signed t)       = ppr t
  ppr (FloatType t)    = ppr t
  ppr Bool             = text "bool"

instance Pretty BinOp where
  ppr Backtick  = text "``"
  ppr Plus      = text "+"
  ppr Minus     = text "-"
  ppr Pow       = text "**"
  ppr Times     = text "*"
  ppr Divide    = text "/"
  ppr Mod       = text "%"
  ppr Quot      = text "//"
  ppr Rem       = text "%%"
  ppr ShiftR    = text ">>"
  ppr ZShiftR   = text ">>>"
  ppr ShiftL    = text "<<"
  ppr Band      = text "&"
  ppr Xor       = text "^"
  ppr Bor       = text "|"
  ppr LogAnd    = text "&&"
  ppr LogOr     = text "||"
  ppr Equal     = text "=="
  ppr NotEqual  = text "!="
  ppr Less      = text "<"
  ppr Leq       = text "<="
  ppr Greater   = text ">"
  ppr Geq       = text ">="
  ppr PipeLeft  = text "<|"
  ppr PipeRight = text "|>"
