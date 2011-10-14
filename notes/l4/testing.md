% Testing

# The dominant paradigm

By far the most widely used style of testing today is _unit_ testing.

* Invent a "state of the world".

* Run the unit we're testing (e.g. a function).

* Check the modified state of the world to see if it looks like it
  should.
  
* Profit!


# Shameless Wikipedia pillaging

If you're used to unit testing, this may look reasonable.

~~~~ {.java}
public class TestAdder {
    public void testSum() {
        Adder adder = new AdderImpl();
        assert(adder.add(1, 1) == 2);
        assert(adder.add(1, 2) == 3);
        assert(adder.add(2, 2) == 4);
        assert(adder.add(0, 0) == 0);
        assert(adder.add(-1, -2) == -3);
        assert(adder.add(-1, 1) == 0);
        assert(adder.add(1234, 988) == 2222);
    }
}
~~~~

The "real world" adds tons of cruft: mock objects, preprocessor abuse
to expose "test seams", etc.


# Problem: unit testing is only as good as your patience

The example from the previous slide contains 7 tests.

It's not hard to imagine a world in which we lose the will to continue
inventing new unit tests long before we've exhausted our search of the
space of possible bugs.


# Let's sort a list

~~~~ {.haskell}
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort pred = go
  where
    go []  = []
    go [x] = [x]
    go xs  = merge (go xs1) (go xs2)
      where (xs1,xs2) = split xs

    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | pred x y  = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
~~~~ {.haskell}


# The split function

The purpose of splitting is to produce two sublists of roughly equal
length, so that they can be merged (where the sorting occurs).

~~~~ {.haskell}
split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:xs,y:ys)
  where (xs,ys) = split zs
~~~~ {.haskell}


# Sorting: a unit test perspective

~~~~
sort [1,2,3,4] == [1,2,3,4]
sort [4,3,2,1] == [1,2,3,4]
sort [1,4,2,3] == [1,2,3,4]
...
~~~~

This gets a little dull.


# Let's talk about properties

A property is nothing more than a predicate that should always hold.

What's an obvious property for sorts?

~~~~ {.haskell}
t_idempotent = sort . sort == sort
~~~~

We can't define this, since we can't compare functions for equality.

However, we can cheat:

~~~~ {.haskell}
t_idempotent xs = 
    sort (sort xs) == sort xs
~~~~


# Why don't we do it in the interpreter

~~~~ {.haskell}
t_idempotent [3,2,1]
~~~~

Okay!  Let's mechanize some exhaustive testing.

To exhaustively test over all lists containing the above elements, we
need some help.

~~~~ {.haskell}
import Data.List (permutations)

t_permute :: ([a] -> Bool) -> [a] -> Bool
t_permute prop = all prop . permutations
~~~~

Over what list sizes is this practical?


# Exhausted testing?

Clearly, exhaustive testing is impractical for most interesting
purposes.

The insight of QuickCheck:

* Combine property-based testing with *randomly generated* data.

* This obviously can't give us the same assurance as exhaustive
  testing, but it's *practical*.
  
* We can choose the amount of data to throw at our properties.

* This lets us tune the tradeoff between *degree of assurance* and
  *cost*.


# Let's give it a try

At the `ghci` prompt:

~~~~
>> import Test.QuickCheck
>> quickCheck (t_idempotent compare)
+++ OK, passed 100 tests.
~~~~

As the output suggests, QuickCheck generated 100 random lists, and
tested our property over them.

Yay!


# Something funky is going on

The type of `compare` is polymorphic:

~~~~
>> :type t_idempotent compare
t_idempotent compare :: Ord a => [a] -> Bool
~~~~

So how are we able to generate a list?


# Type defaulting - a recap

Haskell's usual defaulting rules take each group of constraints `(C1 a, C2
a, ..., Cn a)` for each type variable `a`, and defaults the type
variable if all of the following conditions hold:

* The type variable `a` appears in no other constraints.

* All the classes `Ci` are standard.

* At least one of the classes `Ci` is numeric.


# That's not enough for us lazy programmers!

To reduce the number of types we're forced to specify by hand, `ghci`
relaxes the standard rules (changes in italics):

* The type variable `a` appears in no other constraints. *Unchanged*.

* All the classes `Ci` are standard, *and all are single-parameter
  type classes*.

* At least one of the classes `Ci` is numeric, *or is `Show`, `Eq`, or
  `Ord`*.

It also adds another critical step when defaulting:

* The type `()` becomes the first of the the standard list of types
  tried when doing type defaulting.


# Peek inside

We can use the `verboseCheck` function to see all the test data that
QuickCheck is generating for us.

~~~~
>> verboseCheck (t_idempotent compare)
~~~~

Notice that we have endless lists of `()`?

Our supposedly reassuring test isn't very useful!


# Polymorphic testing

When we're testing a polymorphic property, we need to specify which
concrete type we're testing at.

~~~~
>> import Data.Word (Word8)
>> verboseCheck (t_idempotent (compare :: Word8 -> Word8 -> Ordering))
~~~~

That's verbose and unhappy-making, right?


# Enter the type synonym

Here's one approach to reducing our work:

~~~~ {.haskell}
type Cmp a = a -> a -> Ordering
~~~~

Trying this at the `ghci` prompt:

~~~~
>> import Data.Word (Word8)
>> verboseCheck (t_idempotent (compare :: Cmp Word8))
~~~~


# Witness the fitness

Here's an alternative approach:

~~~~
t_witnessed p a xs = mergeSort p (mergeSort p xs) == mergeSort p xs
  where _witness = a < head xs
~~~~

What's that `_witness` variable for?

* It's a *type witness*, a value that exists to express a constraint
  between several types (it "witnesses" the constraint).
  
* Thanks to the use of `<`, this witness forces the type of `a` and
  the type of the elements of `xs` to be the same.

(We prefix the name with an underscore to tell GHC that it's an unused
wild card.)


# Instantiating our new polymorphic test

We can supply a value for `a` of the appropriate type to test over:

~~~~
>> verboseCheck (t_witnessed compare 'a')
~~~~

Of course, the value of `a` is never used.

As a result, we don't even need to supply a working value, provided the
type of what we supply is correct:

~~~~
>> verboseCheck (t_witnessed compare (undefined::Double))
~~~~


# Where do random values come from?

To generate random values of some type, we must write an `Arbitrary`
instance for it.

~~~~ {.haskell}
class Arbitrary a where
  arbitrary :: Gen a
~~~~

Here's an example, making use of the fact that this unknown type `Gen`
is an instance of `Monad`:

~~~~ {.haskell}
data Point = Point Int Int

instance Arbitrary Point where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Point x y)
~~~~


# Lifting

We're hopefully by now familiar with the `Functor` class:

~~~~ {.haskell}
class Functor f  where
    fmap :: (a -> b) -> f a -> f b
~~~~

This takes a pure function and "lifts" it into the functor `f`.

In general, "lifting" takes a concept and transforms it to work in a
different (sometimes more general) setting.

For instance, we can define lifting of functions with the `Monad`
class too:

~~~~ {.haskell}
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f action = do
  b <- action
  return (f b)
~~~~


# fmap and liftM

Notice the similarities between the type signatures:

~~~~ {.haskell}
fmap  :: (Functor f) => (a -> b) -> f a -> f b
liftM :: (Monad m)   => (a -> b) -> m a -> m b
~~~~

All instances of `Monad` can possibly be instances of `Functor`.
Ideally, they'd be defined in terms of each other:

~~~~ {.haskell}
class (Functor m) => Monad m where
    {- blah blah -}
~~~~

For historical reasons, the two classes are separate, so we write
separate instances for them and just reuse the code:

~~~~ {.haskell}
instance Monad MyThingy where
    {- whatever -}
    
instance Functor MyThingy where
    fmap = liftM
~~~~


# Why the apparent digression?

It turns out that lifting pure functions into monads is really common.

So common, in fact, that `Control.Monad` defines a bunch of extra
combinators for us.

~~~~ {.haskell}
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m b
liftM2 f act1 act2 = do
  a <- act1
  b <- act2
  return (f a b)
~~~~

These combinators go all the way up to `liftM5`.

Look familiar? Useful?


# A tighter Arbitrary instance

Before:

~~~~ {.haskell}
instance Arbitrary Point where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Point x y)
~~~~

After:

~~~~ {.haskell}
import Control.Monad (liftM2)

instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary
~~~~


# Trying something else

Suppose we want to verify that the sum of two odd integers is always
even.

Here's a quick and obvious property:

~~~~ {.haskell}
p_sum_odd a b
    | odd a && odd b = even (a + b)
    | otherwise      = True
~~~~

This looks a little crufty, though.

* We're returning `True` even in instances where the property really
  isn't defined.


# Conditional properties

It would be nice if we could express the idea "check this property
only if the inputs satisfy these constraints".

In fact, there's a combinator for that: `==>`

~~~~ {.haskell}
p_sum_odd1 a b =
    odd a && odd b ==> 
    even (a+b)
~~~~

This specifies that the property on the right should hold whenever the
`Bool`-valued test on the left succeeds.

QuickCheck will discard inputs for which the test fails.


# Limits on conditional properties

Suppose we try to test this property:

~~~~ {.haskell}
p_odd3 a b c = odd a && odd b && odd c ==> odd (a+b+c)
~~~~

We'll get a strange error from QuickCheck:

~~~~
>> quickCheck p_odd
*** Gave up! Passed only 79 tests.
~~~~

None of our tests failed, but QuickCheck puts an upper limit on the
number of test cases it will generate.

* This avoids an infinite loop if the condition before the property
  never holds.

* Here, we hit the limit because almost 90% of our inputs are
  discarded.  Why are they discarded?
  
  
# Correctness by construction

Instead of filtering out data that isn't right for us, it's better to
generate *only* data that is right.

~~~~
newtype Odd a = Odd a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Odd a) where
    arbitrary = do
      a <- arbitrary
      return $! Odd (if even a then a + 1 else a)
~~~~

It's clear from inspection that the `Arbitrary` instance for `Odd a`
will only generate odd-valued integers.


# Can I get a witness?

We no longer need a conditional property now, since all of our
property's parameters must by necessity be odd:

~~~~ {.haskell}
p_odd1 x (Odd a) (Odd b) (Odd c) = odd (a+b+c)
  where _witness = x == a
~~~~

Let's run it:

~~~~
>> quickCheck $ p_odd1 (1::Int)
+++ OK, passed 100 tests.
~~~~

Sweet! We saved ourselves from generating a ton of useless data, made
the testing process faster, and cleaned up the property some too.


# Sizing a test

Test data generators have an implicit size parameter, hidden inside
the `Gen` type.

QuickCheck starts by generating small test cases; it increases the
size as testing progresses.

The meaning of "size" is specific to the needs of an `Arbitrary`
instance.

* The `Arbitrary` instance for lists interprets it as "the maximum
  length of a list of arbitrary values".

We can find the current size using the `sized` function, and modify it
locally using `resize`:

~~~~ {.haskell}
sized  :: (Int -> Gen a) -> Gen a
resize ::  Int -> Gen a  -> Gen a
~~~~


# Testing a recursive data type

Suppose we have a tree type:

~~~~ {.haskell}
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)
~~~~

Here's an obvious `Arbitrary` instance:

~~~~ {.haskell}
instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [
                  liftM Leaf arbitrary
                , liftM2 Node arbitrary arbitrary
                ]
~~~~

The `oneof` combinator chooses a generator at random.

~~~~ {.haskell}
oneof :: [Gen a] -> Gen a
~~~~


# What's up, Doc?

Potential trouble:

* This generator may not terminate at all!

* It's likely to produce *huge* trees.

We can use the `sample` function to generate and print some arbitrary
data.

~~~~ {.haskell}
sample :: (Show a) => Gen a -> IO ()
~~~~

This helps us to explore what's going on.


# A safer instance

Here's where the sizing mechanism comes to the rescue.

~~~~ {.haskell}
instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = sized tree

tree :: (Arbitrary a) => Int -> Gen (Tree a)
tree 0 = liftM Leaf arbitrary
tree n = oneof [
           liftM  Leaf arbitrary
         , liftM2 Node subtree subtree
         ]
  where subtree = tree (n `div` 2)
~~~~


# Sorting revisited

A very desirable property of a sorting algorithm is *stability*.

The built-in `sort` and `sortBy` functions are designed to be stable.

What about our `mergeSort`? Let's find out.

~~~~ {.haskell}
import Data.Function (on)
import Data.List (sortBy)
import Data.Word (Word8)

p_stable xs = merged == sorted
  where merged = mergeSort (compare `on` fst) xs
        sorted = sortBy    (compare `on` fst) xs
        _witness = xs :: [(Word8, Word8)]
~~~~

That `on` function is pretty neat:

~~~~ {.haskell}
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y
~~~~
