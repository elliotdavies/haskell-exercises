{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises06 where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (x :: Nat) + (y :: Nat) where
  'Z     + y = y
  ('S x) + y = 'S (x + y)

-- | b. Write a type family '*' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (x :: Nat) * (y :: Nat) where
  'Z     * y = 'Z
  ('S x) * y = (x * y) + y

--    UndecidableInstances, because the type family is now recursive and
--    there's no guarantee it will ever terminate

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.

addSNat :: SNat x -> SNat y -> SNat (x + y)
addSNat SZ y     = y
addSNat (SS x) y = SS (addSNat x y)




{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

instance Show a => Show (Vector c a) where
  show VNil = "Nil"
  show (VCons x xs) = show x ++ " " ++ show xs


-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil y         = y
append (VCons x xs) y = VCons x (append xs y)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

-- This works although does reverse the list
flatMap :: (a -> Vector m b) -> Vector n a -> Vector (n * m) b
flatMap _ VNil          = VNil
flatMap f (VCons x xs)  = append (flatMap f xs) (f x)




{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (a :: Bool) && (b :: Bool) :: Bool where
  'True && 'True = 'True
  a     && b     = 'False

-- | b. Write the type-level @||@ function for booleans.

type family (a :: Bool) || (b :: Bool) :: Bool where
  'False || 'False = 'False
  a      || b      = 'True

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family All (xs :: [Bool]) :: Bool where
  All (x ': '[])  = x
  All (x ': xs)   = x && All xs



{- FOUR -}

-- | a. Nat fun! Write a type-level '<=' function using the promoted 'Ordering'
-- type.

type family (a :: Nat) <= (b :: Nat) :: Bool where
  'Z    <=  b   = 'True
  a     <= 'Z   = 'False
  'S a  <= 'S b = a <= b

    -- Not sure where the Ordering type comes into this

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max 'Z b = b
  Max a 'Z = a
  Max ('S a) ('S b) = 'S (Max a b)

-- | c. Write a family to get the maximum natural in a list.

type family MaxInList (xs :: [Nat]) :: Nat where
  MaxInList (x ': '[]) = x
  MaxInList (x ': xs) = Max x (MaxInList xs)




{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family InsertT (n :: Nat) (t :: Tree) :: Tree where
  InsertT n 'Empty = 'Node 'Empty n 'Empty
  -- Didn't say where to insert, so let's always go left for now
  InsertT n ('Node l v r) = 'Node (InsertT n l) v r



{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.

type family DeleteT (n :: Nat) (t :: Tree) :: Tree where
  DeleteT _ 'Empty = 'Empty
  -- Delete all occurrences of `n`; join together any dangling subtrees
  DeleteT n ('Node l n r) = AppendT (DeleteT n l) (DeleteT n r)
  DeleteT n ('Node l v r) = 'Node (DeleteT n l) v (DeleteT n r)

type family AppendT (a :: Tree) (b :: Tree) :: Tree where
  AppendT a 'Empty = a
  AppendT 'Empty b = b
  AppendT ('Node l v r) b = 'Node (AppendT l b) v r -- Again, go left for now


{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.

type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

appendH :: HList xs -> HList ys -> HList (xs ++ ys)
appendH HNil y = y
appendH (HCons x xs) y = HCons x (appendH xs y)



{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every _    '[]       = ()
  Every ctor (x ': xs) = CAppend (ctor x) (Every ctor xs)

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

instance Every Show xs => Show (HList xs) where
  show HNil = ""
  show (HCons x xs) = show x ++ " " ++ show xs

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?

instance Every Eq xs => Eq (HList xs) where
  HNil       == HNil       = True
  HCons x xs == HCons y ys = x == y && xs == ys

instance (Every Eq xs, Every Ord xs) => Ord (HList xs) where
  HNil `compare` HNil = EQ
  HCons x xs `compare` HCons y ys = x `compare` y <> xs `compare` ys

    -- Yes, the behaviour of needing Eq to define Ord was expected



{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

type family Calc (n :: Nat) :: [Nat] where
  Calc 'Z     = 'Z   ': '[]
  Calc ('S n) = 'S n ': Calc n

-- | b. Write a type-level prime number sieve.

  -- This doesn't sound fun, to be honest

-- | c. Why is this such hard work?
