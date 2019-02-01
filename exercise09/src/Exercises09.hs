{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises09 where

import Data.Kind (Constraint, Type)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (TypeError, ErrorMessage(..))




{- ONE -}

-- | Consider the following types:

newtype MyInt   = MyInt   Int
newtype YourInt = YourInt Int

-- | As Haskell programmers, we love newtypes, so it would be super useful if
-- we could define a class that relates a newtype to the type it wraps, while
-- also giving us functions to get between them (we can call them 'wrap' and
-- 'unwrap').

-- | a. Write the class!

class Newtype a b | b -> a where
   wrap   :: a -> b
   unwrap :: b -> a

-- | b. Write instances for 'MyInt' and 'YourInt'.

instance Newtype Int MyInt where
  wrap             = MyInt
  unwrap (MyInt i) = i

instance Newtype Int YourInt where
  wrap               = YourInt
  unwrap (YourInt i) = i

-- | c. Write a function that adds together two values of the same type,
-- providing that the type is a newtype around some type with a 'Num' instance.

addNewtypes :: (Num a, Newtype a b) => b -> b -> b
addNewtypes x y = wrap (unwrap x + unwrap y)

-- | d. We actually don't need @MultiParamTypeClasses@ for this if we use
-- @TypeFamilies@. Look at the section on associated type instances here:
-- https://wiki.haskell.org/GHC/Type_families#Associated_type_instances_2 -
-- rewrite the class using an associated type, @Old@, to indicate the
-- "unwrapped" type. What are the signatures of 'wrap' and 'unwrap'?

class Newtype' new where
  type Old new :: Type

  wrap' :: Old new -> new
  unwrap' :: new -> Old new



{- TWO -}

-- | Who says we have to limit ourselves to /types/ for our parameters? Let's
-- look at the definition of 'traverse':

traverse1 :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse1 = traverse

-- | This is all very well, but we often don't need @f@ to be an 'Applicative'.
-- For example, let's look at the good ol' 'Identity' type:

newtype Identity a = Identity a
  deriving Functor -- LANGUAGE DeriveFunctor

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

-- | We can see that, in the @Traversable@ instance, we don't actually use
-- @pure@ /or/ @(<*>)@ - we only use @<$>@! It would be nice if we could have a
-- better @Traversable@ class that takes both the @t@ type /and/ the constraint
-- we want on the @f@...

-- | a. Write that little dazzler! What error do we get from GHC? What
-- extension does it suggest to fix this?

class (Functor t, Foldable t) => Wanderable c t  where
  wander :: c f => (a -> f b) -> t a -> f (t b)

  -- Need to AllowAmbiguousTypes


-- | b. Write a 'Wanderable' instance for 'Identity'.

instance Wanderable Functor Identity where
  wander f (Identity x) = Identity <$> f x


-- | c. Write 'Wanderable' instances for 'Maybe', '[]', and 'Proxy', noting the
-- differing constraints required on the @f@ type.

instance Wanderable Applicative Maybe where
  wander f (Just x) = Just <$> f x
  wander f Nothing  = pure Nothing

instance Wanderable Applicative [] where
  wander f = foldr go (pure [])
    where
      go x xs = (:) <$> f x <*> xs

-- | d. Assuming you turned on the extension suggested by GHC, why does the
-- following produce an error? Using only the extensions we've seen so far, how
-- could we solve this, perhaps in a way that involves another parameter to the
-- 'wander' function? A parameter whose type could be annotated? (Don't worry -
-- we'll see in later chapters that there are neater solutions to this
-- problem!)

-- test = wander Just [1, 2, 3]

  -- There's nothing here to specify the `c f` constraint, but we could solve it
  -- with type annotations (or proxies)





{- THREE -}

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ ::           SNat  'Z
  SS :: SNat n -> SNat ('S n)

-- | In the @DataKinds@ chapter, we wrote the 'SmallerThan' data type:

data SmallerThan (limit :: Nat) where
  SmallerThanZ ::                  SmallerThan ('S n)
  SmallerThanS :: SmallerThan n -> SmallerThan ('S n)

-- | We can write a class to take an 'SNat' to a 'SmallerThan' using
-- @MultiParamTypeClasses@. We can even use @TypeOperators@ to give our class a
-- more intuitive name:

class (x :: Nat) < (y :: Nat) where
  convert :: SNat x -> SmallerThan y

-- | a. Write the instance that says @Z@ is smaller than @S n@ for /any/ @n@.

instance Z < S n where
  convert SZ = SmallerThanZ

-- | b. Write an instance that says, if @x@ is smaller than @y@, then @S x@ is
-- smaller than @S y@.

instance (x < y) => S x < S y where
  convert (SS sx) = SmallerThanS $ convert sx

-- | c. Write the inverse function for the class definition and its two
-- instances.

class (x :: Nat) > (y :: Nat) where
  convert' :: SmallerThan x -> SNat y

instance S n > Z where
  convert' SmallerThanZ = SZ

instance (y > x) => S y > S x where
  convert' (SmallerThanS sy) = SS (convert' sy)




{- FOUR -}

-- | In a couple places, we've seen the @(~)@ (or "equality") constraint being
-- used. Essentially, we can think of it as a two-parameter typeclass with one
-- instance.

-- | a. Write that typeclass!

class Equal a b where
  -- Part c
  fromE :: a -> b
  toE   :: b -> a

-- | b. Write that instance!

instance Equal a a where
  -- Part c
  fromE = Prelude.id
  toE   = Prelude.id

-- | c. When GHC sees @x ~ y@, it can apply anything it knows about @x@ to @y@,
-- and vice versa. We don't have the same luxury with /our/ class, however –
-- because we can't convince the compiler that only one instance will ever
-- exist, it can't assume that we want the instance we've just written. No
-- matter, though - we can just add two functions to our class to convert
-- between the types. Write them, and don't overthink!


-- | d. GHC can see @x ~ y@ and @y ~ z@, then deduce that @x ~ z@. Can we do
-- the same? Perhaps with a second instance? Which pragma(s) do we need and
-- why?

instance (Equal a b, Equal b c) => Equal a c
  -- ???



{- FIVE -}

-- | It wouldn't be a proper chapter without an @HList@, would it?

data HList (xs :: [Type]) where
  -- In fact, you know what? You can definitely write an HList by now – I'll
  -- just put my feet up and wait here until you're done!
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | We know we can use type families to append type-level lists...

type family (ls :: [k]) ++ (ys :: [k]) :: [k] where -- Oo, @PolyKinds@!
  '[     ]  ++ ys =             ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Notice that we pattern-match on the first argument, but don't even need
-- to look into the second one. A CLUE, HOLMES.

-- | Consider the following class:

class Appendable (ls :: [Type]) (rs :: [Type]) where
  append :: HList ls -> HList rs -> HList (ls ++ rs)

-- | a. Write an instance for an empty left-hand HList.

instance Appendable '[] rs where
  append HNil rs = rs

-- | b. Write an instance for a non-empty left-hand list. You "may" need a
-- constraint on this instance.

instance Appendable ls rs => Appendable (l ': ls) rs where
  append (HCons l ls) rs = HCons l $ append ls rs

-- | c. What does this tell us about the functionality that type classes can
-- "add" to type families?

  -- They don't add anything at the type level, but they can help implement
  -- the logic at the value level?




{- SIX -}

-- | We could also imagine a type class to "pluck" types out of @HList@:

class Pluck (x :: Type) (xs :: [Type]) where
  pluck :: HList xs -> x

-- | a. Write an instance for when the head of @xs@ is equal to @x@.

instance {-# OVERLAPPING #-} Pluck x (x ': xs) where
  pluck (HCons x xs) = x

-- | b. Write an instance for when the head /isn't/ equal to @x@.

instance Pluck x ys => Pluck x (y ': ys) where
  pluck (HCons y ys) = pluck ys

-- | c. Using [the documentation for user-defined type
-- errors](http://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-TypeLits.html#g:4)
-- as a guide, write a custom error message to show when you've recursed
-- through the entire @xs@ list (or started with an empty @HList@) and haven't
-- found the type you're trying to find.

instance TypeError (Text "No more HList left :-(") => Pluck x '[] where
  pluck = error "oops"

-- | d. Making any changes required for your particular HList syntax, why
-- doesn't the following work? Hint: try running @:t 3@ in GHCi.

--mystery :: Int
--mystery = pluck (HCons 3 HNil)

  -- Can't be certain 3 is an Int here - could be some other Num instance



{- SEVEN -}

-- | A variant is similar to an 'Either', but generalised to any non-zero
-- number of parameters. Typically, we define it with two parameters: @Here@
-- and @There@. These tell us which "position" our value inhabits:

-- variants :: [Variant '[Bool, Int, String]]
-- variants = [ Here True, There (Here 3), There (There (Here "hello")) ]

-- | a. Write the 'Variant' type to make the above example compile.

data Variant (xs :: [Type]) where
  -- Here  :: ...
  -- There :: ...

-- | b. The example is /fine/, but there's a lot of 'Here'/'There' boilerplate.
-- Wouldn't it be nice if we had a function that takes a type, and then returns
-- you the value in the right position? Write it! If it works, the following
-- should compile: @[inject True, inject (3 :: Int), inject "hello"]@.

-- class Inject … … where
--   inject :: …

-- | c. Why did we have to annotate the 3? This is getting frustrating... do
-- you have any (not necessarily good) ideas on how we /could/ solve it?





{- EIGHT -}

-- | As engineers, we are wont to over-think day-to-day problems in order to
-- justify our existence to scrum masters. As such, we are compelled to visit
-- our friendly neighbourhood angel investor with a new idea: given the weather
-- and rough temperature, our web2.0, blockchain-ready app - chil.ly - will
-- tell you whether or not you need a coat. Let's start by defining our inputs:

data Weather     = Sunny | Raining
data Temperature = Hot   | Cold

-- ... and some singletons, why not?

data SWeather (w :: Weather) where
  SSunny   :: SWeather 'Sunny
  SRaining :: SWeather 'Raining

data STemperature (t :: Temperature) where
  SHot  :: STemperature 'Hot
  SCold :: STemperature 'Cold

-- | Now, our app is going to be ready-for-scale, B2B, and proven with zero
-- knowledge, so we want type safety /at the core/. Naturally, we've defined
-- the relationship between the two domains as a type class.

class Coat (a :: Weather) (b :: Temperature) where
  doINeedACoat :: SWeather a -> STemperature b -> Bool

-- | It's early days, and we're just building an MVP, but there are some rules
-- that /everyone/ knows, so they should be safe enough!

-- No one needs a coat when it's sunny!
instance Coat Sunny b where doINeedACoat _ _ = False

-- It's freezing out there - put a coat on!
instance Coat a Cold where doINeedACoat _ _ = True

-- | Several months pass, and your app is used by billions of people around the
-- world. All of a sudden, your engineers encounter a strange error:

-- test :: Bool
-- test = doINeedACoat SSunny SCold

-- | Clearly, our data scientists never thought of a day that could
-- simultaneously be sunny /and/ cold. After months of board meetings, a
-- decision is made: you /should/ wear a coat on such a day. Thus, the
-- __second__ rule is a higher priority.

-- | a. Uncomment the above, and add OVERLAPPING and/or OVERLAPPABLE pragmas
-- to prioritise the second rule. Why didn't that work? Which step of the
-- instance resolution process is causing the failure?

-- | b. Consulting the instance resolution steps, which pragma /could/ we use
-- to solve this problem? Fix the problem accordingly.

-- | c. In spite of its scary name, can we verify that our use of it /is/
-- undeserving of the first two letters of its name?





{- NINE -}

-- | The 'Show' typeclass has two instances with which we're probably quite
-- familiar:

-- instance Show a => Show [a]
-- instance           Show String

-- | a. Are these in conflict? When?

-- | b. Let's say we want to define an instance for any @f a@ where the @f@ is
-- 'Foldable', by converting our type to a list and then showing that. Is there
-- a pragma we can add to the first 'Show' instance above so as to preserve
-- current behaviour? Would we need /more/ pragmas than this?

-- | c. Somewhat confusingly, we've now introduced incoherence: depending on
-- whether or not I've imported this module, 'show' will behave in different
-- ways. Your colleague suggests that your use of pragmas is the root issue
-- here, but they are missing the bigger issue; what have we done? How could we
-- have avoided it?





{- TEN -}

-- | Let's imagine we have some types in our codebase:

newtype UserId = UserId Int

data User
  = User
      { id      :: UserId
      , knownAs :: String
      }

newtype CommentId = CommentId Int

data Comment
  = Comment
      { id     :: CommentId
      , author :: Int
      , text   :: String
      }

data Status = Blocked | Deleted

-- | In order to better facilitate mobile devices, we now want to introduce
-- caching. I start work, and eventually slide a pull request into your DMs:

class UserCache where
  storeUser :: User -> Map UserId User -> Map UserId User
  loadUser :: Map UserId User -> UserId -> Either Status User

class CommentCache where
  storeComment :: Comment -> Map CommentId Comment -> Map CommentId Comment
  loadComment  :: Map CommentId Comment -> CommentId -> Maybe Comment

-- | "This is silly", you exclaim in a supportive and non-hostile way. "These
-- classes only differ in three ways! We could write this as a multi-parameter
-- type class!", you suggest in a way that encourages my continued progression
-- as an engineer and team player.

-- | a. What are those three ways? Could we turn them into parameters to a
-- typeclass? Do it!

-- | b. Write instances for 'User' and 'Comment', and feel free to implement
-- them as 'undefined' or 'error'. Now, before uncommenting the following, can
-- you see what will go wrong?

-- oops cache = load cache (UserId (123 :: Int))

-- | c. Do we know of a sneaky trick that would allow us to fix this? Possibly
-- involving constraints? Try!
