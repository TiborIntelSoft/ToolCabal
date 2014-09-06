module Tools.DSet where

import Data.Set (Set)
import qualified Data.Set as S
import Data.List (elemIndices)

-- * Data types

{- | 
  The DSet data type descripes a dependency specification
  of a set of items.
-}
data DSet a 
  -- | This is just the value lifted to a DSet
  = Base a
  -- | This forces a minimum number of constraints to be satisfied
  | AtLeastN Int (Set (DSet a))
  -- | This forces an exact number of constraints to be satisfied
  -- No less, no more is allowed
  | ExactN Int (Set (DSet a))
  -- | This forces a maximum number of constraints to be satisfied
  | AtMostN Int (Set (DSet a))
  -- | Dependent a ds means that if a is present in the set then 
  -- the constraint of ds should hold. If a is not present what the ds is
  -- does not matter.
  | Dependent a (DSet a)
  -- | This is the constraint that the set should be empty
  | ForceEmpty
  deriving (Show, Read, Ord, Eq)

-- * Construction

-- | Lifts a value to a DSet
toDSet :: a -> DSet a
toDSet = Base

-- | The empty set constraint
forceEmptyDSet :: DSet a
forceEmptyDSet = ForceEmpty

-- | The constraint that is always satisfied. It is defined as
-- the unit for @dsetOr@
emptyDSet :: DSet a
emptyDSet = AtLeastN 0 S.empty

-- | Generalization for the dependency constraint construction
dependWrap :: Ord a => (Set a -> DSet a) -> a -> Set a -> DSet a
dependWrap f a = Dependent a . f

-- | The value depends on a single element of the set
dependOnOne :: Ord a => a -> Set a -> DSet a
dependOnOne = dependWrap oneof

-- | The value depends on all elements of the set
dependOnAll :: Ord a => a -> Set a -> DSet a
dependOnAll = dependWrap required

-- | Generalization for the multiple dependency constraint construction
dependAllWrap :: Ord a => (a ->  Set a -> DSet a) -> Set a -> Set a -> DSet a
dependAllWrap f a s = ExactN (S.size a) $ S.map (flip f s) a

-- | All values in the first set depend on a single element of the second set
dependAllOnOne :: Ord a => Set a -> Set a -> DSet a
dependAllOnOne = dependAllWrap dependOnOne

-- | All values in the first set depend on all elements of the second set
dependAllOnAll :: Ord a => Set a -> Set a -> DSet a
dependAllOnAll = dependAllWrap dependOnAll

-- | Generalization for converting a set to a dset
wrap :: Ord a => (Set (DSet a) -> DSet a) ->  Set a -> DSet a
wrap f = f . S.map toDSet

-- | The constraint that at least one element should be present
oneof :: Ord a => Set a -> DSet a
oneof = wrap $ AtLeastN 1

-- | The constraint that all elements in the set should be present
required :: Ord a => Set a -> DSet a
required s = wrap (ExactN (S.size s)) s

-- | The optional constraint. This is always satisfied.
-- The usefulness lies in that it declares allowed flags
optional :: Ord a => Set a -> DSet a
optional = wrap $ AtLeastN 0

-- | Only one of the set may be present and it is required that 
-- at least one element of the set is present
uniqueRequired :: Ord a => Set a -> DSet a
uniqueRequired = wrap $ ExactN 1

-- | The constraint that only one element of the set may be present
uniqueOptional :: Ord a => Set a -> DSet a
uniqueOptional = wrap $ AtMostN 1

-- * Combining DSets
-- | The constraint that tells that both constraints should be satisfied.
-- If possible it fuses the constraints
dsetAnd :: Ord a => DSet a -> DSet a -> DSet a
dsetAnd (ExactN n sfs1) (ExactN m sfs2) | n == S.size sfs1 && m == S.size sfs2 = ExactN (n+m) $ S.union sfs1 sfs2
dsetAnd (ExactN n sfs) fs | n == S.size sfs = ExactN (n+1) $ S.insert fs sfs
dsetAnd fs (ExactN n sfs) | n == S.size sfs = ExactN (n+1) $ S.insert fs sfs
dsetAnd fs1 fs2 = ExactN 2 $ S.fromList [fs1, fs2]

-- | The constraint that tells that at least one of the constraints should be satisfied.
-- If possible it fuses the constraints
dsetOr :: Ord a => DSet a -> DSet a -> DSet a
dsetOr (AtLeastN n sfs1) (AtLeastN m sfs2) | n == 1 && m == 1 = AtLeastN 1 $ S.union sfs1 sfs2
                                        | n == 0 || m == 0 = AtLeastN 0 $ S.union sfs1 sfs2
dsetOr (AtLeastN 1 sfs) fs = AtLeastN 1 $ S.insert fs sfs
dsetOr fs (AtLeastN 1 sfs) = AtLeastN 1 $ S.insert fs sfs
dsetOr fs1 fs2 = AtLeastN 1 $ S.fromList [fs1, fs2]

-- | The constraint that tells that only one constraint may be satisfied.
dsetXor ::Ord a => DSet a -> DSet a -> DSet a
dsetXor fs1 fs2 = ExactN 1 $ S.fromList [fs1, fs2]

-- * Operators

-- | Operator for @dsetAnd@
infixr 5 <&&>
(<&&>) :: Ord a => DSet a -> DSet a -> DSet a
(<&&>) = dsetAnd

-- | Operator for @dsetOr@
infixr 4 <||>
(<||>) :: Ord a => DSet a -> DSet a -> DSet a
(<||>) = dsetOr

-- | Operator for @dsetXor@
infixr 4 <*|>
(<*|>) :: Ord a => DSet a -> DSet a -> DSet a
(<*|>) = dsetXor

-- * Conversion

-- | This retrieves all the allowed items from the constraint set
dSetToSet :: Ord a => DSet a -> Set a
dSetToSet (Base s) = S.singleton s
dSetToSet (AtLeastN _ s) = S.unions $ map dSetToSet $ S.toList s
dSetToSet (ExactN _ s) = S.unions $ map dSetToSet $ S.toList s
dSetToSet (AtMostN _ s) = S.unions $ map dSetToSet $ S.toList s
dSetToSet (Dependent a s) = S.insert a $ dSetToSet s
dSetToSet (ForceEmpty) = S.empty

-- * Validation

-- | This tests if a set satisfies the constraint set
-- It uses as the satisfying function the equality of items
validDSet :: (Ord a, Eq a) => Set a -> DSet a -> Bool
validDSet = validDSet' (==)

-- | This tests if a set satisfies the constraint set. It also
-- tests if all supplied items are allowed by the constraint set
-- and if all the constraints of the constraint set are satisfied.
-- It gets an additional function that tells when an item in the set
-- corresponds with an item in the dset (both for satisfying a constraint
-- as for checking if the item is allowed by the constraint set).
validDSet' :: Ord a => (a -> a -> Bool) -> Set a -> DSet a -> Bool
validDSet' f s d = validDSetItems f s d && validDSetSet f s d

-- | This checks if all the items are allowed by the constraint set
validDSetItems :: Ord a => (a -> a -> Bool) -> Set a -> DSet a -> Bool
validDSetItems c s d = and $ map f $ S.toList s
  where ds = dSetToSet d
        f x = 0 /= (S.size $ S.filter (c x) ds)

-- | This checks if all the constraints of the constraint set are
-- satisfied by the given set
validDSetSet :: (a -> a -> Bool) -> Set a -> DSet a -> Bool
validDSetSet c s (Base a) = not $ S.null $ S.filter (c a) s
validDSetSet c s (AtLeastN n sfs) = totalSatisfied c sfs s >= n
validDSetSet c s (ExactN n sfs) = totalSatisfied c sfs s == n
validDSetSet c s (AtMostN n sfs) = totalSatisfied c sfs s <= n
validDSetSet c s (Dependent a fs) = not (validDSetSet c s (Base a)) || validDSetSet c s fs
validDSetSet _ s (ForceEmpty) = S.null s

-- | This counts the number of satisfied constraints in a set of constraints
totalSatisfied :: (a -> a -> Bool) -> Set (DSet a) -> Set a -> Int
totalSatisfied c sfs s = length $ elemIndices True $ map (validDSetSet c s) $ S.toList sfs