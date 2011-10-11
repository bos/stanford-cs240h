% Zippers and such

# Back to basics

How many values can we construct from the following type?

~~~~ {.haskell}
data Bool = False | True
~~~~

Note: in this discussion, we're explicitly omitting well-typed but
non-terminating constructs such as the following:

~~~~ {.haskell}
loop :: Bool
loop = loop

wtf :: Bool
wtf = undefined

crash :: Bool
crash = error "fnord"
~~~~


# Ordering

Another well-known type:

~~~~ {.haskell}
data Ordering = LT | EQ | GT
~~~~

Clearly we can construct three different values of this type.


# A zero-valued type

In Haskell 2010, we can create types from which *no* values can be
constructed:

~~~~ {.haskell}
data Empty
~~~~

This type has no value constructors (and we can't use `deriving`
syntax on it).


# Zero, one, two...

So big deal, we can create types with zero or more constructors:

~~~~ {.haskell}
data Empty
~~~~

~~~~ {.haskell}
data One = One
~~~~

~~~~ {.haskell}
data Bool = False | True
~~~~


# Adding some parameters

Another type to ponder.

~~~~ {.haskell}
data A = A Bool
       | B Ordering
~~~~

We can construct five values of this type:

~~~~ {.haskell}
A False
A True
B LT
B EQ
B GT
~~~~


# A different tack

How many values can this type represent?

~~~~ {.haskell}
data Fnord = Fnord Bool Ordering
~~~~

What about this one?

~~~~ {.haskell}
data Quaternion = Point Double Double Double Double
~~~~


# Switching the notation: sums

Let's take a different perspective for a moment, and do some
arithmetic.

~~~~ {.haskell}
data Sum = A Bool
         | B Ordering
~~~~

If we exhaustively enumerate the possible values of this type, we see
that there are as many values as:

* Values of `Bool`...
* ...*added to*...
* Values of `Ordering`

Let's write that number as $\text{Bool} + \text{Ordering}$


# Switching the notation: products

From reading this type:

~~~~ {.haskell}
data Product = Product Bool Ordering
~~~~

Following the previous example, it's pretty clear that we can create
as many values of type `Ordering` as there are:

* Values of `Bool`...
* ...*multiplied by*...
* Values of `TrafficLight`

Let's write that number as $\text{Bool} \times \text{Ordering}$


# From arithmetic to algebra: sums

Now let's introduce polymorphism into the mix.

~~~~ {.haskell}
data Either a b = Left a | Right b
~~~~

We don't know how many values there are of this type, since neither
`a` nor `b` is specified.

But we can still write an algebraic expression that will compute the
right number, once we plug concrete types in: 

* $a+b$


# From arithmetic to algebra: products

This should be a no-brainer:

~~~~ {.haskell}
data Triple a b c = Triple a b c
~~~~

The algebraic expression that describes the number of values of this
type is no surprise:

* $a+b+c$


# Mixing sums and products

How many values are there of this type?

~~~~ {.haskell}
type Foo a b c = Foo a b
               | Bar b c
~~~~


# Clarity on naming: sums

Consider a type that consists only of zero-parameter constructors:

~~~~ {.haskell}
data Rainbow = Red | Orange | Yellow | Green {- etc -}
~~~~

These are often referred to as *sum types*.


# Clarity on naming: products

If a type has only one constructor, and that constructor takes
parameters:

~~~~ {.haskell}
data Point = Point Int Int
~~~~

We refer to this as a *product type*.


# Algebraic data types

Haskell's type system admits sum types, product types, and types that
are a mixture of both.

* This should make it clear why the word "algebraic" appears in the
  phrase.


# The frob merchant

This is a frob.

~~~~
                 ___             ____
                /__/\     ______/___/\
                \  \ \   /          /\\
                 \  \ \_/__        /  \
                 _\  \ \  /\______/__  \
                // \__\/ /  \       /\  \
        _______//_______/    \     / _\_/_____
       /      / \       \    /    / /        /\
    __/      /   \       \  /    / /        / _\__
   / /      /     \_______\/    / /        / /   /\
  /_/______/___________________/ /________/ /___/  \
  \ \      \    ___________    \ \        \ \   \  /
   \_\      \  /          /\    \ \        \ \___\/
      \      \/          /  \    \ \        \  /
       \_____/          /    \    \ \________\/
            /__________/      \    \  /
            \   _____  \      /_____\//
             \ /    /\  \    /    \  /
              /____/  \  \  /______\/\
              \    \  /___\/     \  \ \
               \____\/            \__\/
~~~~


# The frob merchant's web store

Suppose we're building a web app, where we want to send frobs to
customers of our web site.

~~~~ {.haskell}
data Customer = Customer {
      custID :: Int
    , custName :: String
    , custAddress :: Address
    }

newtype Zip = Zip Int

data Address = Address {
      addrStreet :: String
    , addrCity :: String
    , addrState :: String
    , addrZip :: Zip
    }
~~~~


# Oh noes!

A customer has made a mistake in entering their shipping zip code.
They've called us up, irate that we've been unable to fulfil their
urgent frob order.

So. We need to change their zip code.

In a C-like language, this would be easy:

~~~~ {.c}
struct Customer *cust;

/* ... */

cust->custAddress->addrZip = 94043;
~~~~


# Getting at a zip code

Haskell's record syntax automatically defines "accessor" or "getter"
functions for us:

~~~~ {.haskell}
custAddress :: Customer -> Address
addrZip :: Address -> Zip
~~~~

Given a `Customer`, we can obviously use function composition to get
their zip:

~~~~ {.haskell}
custZip :: Customer -> Zip
custZip = addrZip . custAddress
~~~~

Unfortunately, we lack a "good" facility for updating records.  Let's
see what that means.


# Setting a zip code

We need to modify a zip code, but we're working in a pure language, so
clearly a "zip code setter" is going to be a function that returns a
new value that is identical to the previous value except for the zip.

~~~~ {.haskell}
setAddrZip :: Zip -> Address -> Address
~~~~

If we have a new `Address` and we want to "modify" a `Customer`, we
need a similar function:

~~~~ {.haskell}
setCustAddress :: Address -> Customer -> Customer
~~~~

Ultimately, our goal is actually to write this function:

~~~~ {.haskell}
setCustZip :: Zip -> Customer -> Customer
~~~~


# Record update syntax

Along with record syntax, Haskell provides an "update" syntax:

~~~~ {.haskell}
setAddrZip :: Zip -> Address -> Address
setAddrZip zip addr = addr { addrZip = zip }
~~~~

The expression on the right means this:

* Make a copy of `addr`

* All fields in the new value should be the same as in `addr`...

* ...*except* for `addrZip`, which should have the value `zip`


# Does this solve our problem?

Here's the other "setter" function we need, which follows the same
pattern:

~~~~ {.haskell}
setCustAddress :: Address -> Customer -> Customer
setCustAddress addr cust = cust { custAddress = addr }
~~~~

Now we can write that `setCustZip` function we wanted:

~~~~ {.haskell}
setCustZip :: Zip -> Customer -> Customer
setCustZip zip cust =
    setCustAddress (setAddrZip zip (custAddress cust)) cust
~~~~

Trouble is, the above looks much uglier to me than the corresponding
C:

~~~~ {.haskell}
cust->custAddress->addrZip = 94043;
~~~~

Worse, we have to write each of our Haskell "setter" functions *by
hand*. Ugh.


# Is the situation hopeless?

Here are our desiderata:

1. We want to be able to access fields within records.

1. We want to be able to *compose* accesses, so that we can inspect
   fields within records that are themselves fields of records.

1. We want to be able to update fields within records.

1. We want to be able to *compose* updates, so that we can modify
   fields within records that are themselves fields of records.

With Haskell's record syntax, we get #1 and #2, sort of #3 (if we
squint), and definitely not #4.


# Lenses

What we want is a type that behaves something like this:

~~~~ {.haskell}
data Lens rec fld = Lens {
      get :: rec -> fld
    , set :: fld -> rec -> rec
    }
~~~~

This "bundles together" a record type `rec` with a field type `fld`,
so that we know:

* how to get a field out of a record, and

* how to update a field within a record.

(Why the name "lens"? Because it lets us *focus* on a field within a
record.)


# How should lenses behave?

We need three laws to hold for lenses.

If we `put` something into a record, we can `get` it back out.

~~~~ {.haskell}
get l (put l b a) == b 
~~~~

If we `get` something out of a record, and `put` it back in, the
result is identical to the original record.

~~~~ {.haskell}
put l (get l a) a == a
~~~~

Two successive `put` operations must give the same result as a single
`put` of the second value:

~~~~ {.haskell}
put l b1 (put l b2 a) == put l b1 a
~~~~

(We call these properties "laws" because they *must* hold in order for
us to be able to reason about lenses.)


# What does a real lens look like?

The following definitions correspond to those in the
[data-lens](http://hackage.haskell.org/package/data-lens) package.

~~~~ {.haskell}
newtype Lens rec fld = Lens (rec -> Store fld rec)
~~~~

where

~~~~ {.haskell}
data Store fld rec = Store (fld -> rec) fld
~~~~

That's hard to follow, so let's dig in and try to understand.  First,
we'll get rid of the name `Store`, to give the tuple:

~~~~ {.haskell}
(fld -> rec, fld)
~~~~

Then we'll substitute this into the definition of `Lens`:

~~~~ {.haskell}
newtype Lens rec fld = Lens (rec -> (fld -> rec, fld))
~~~~


# Simplifying further

If we ignore all the `newtype` noise, we're left with a very
simple type:

~~~~ {.haskell}
rec -> (fld -> rec, fld)
~~~~

That is, a `Lens` is:

* A function that accepts a record type `rec` as its argument

* It returns a pair

* The first element is a setter: give it a field value of type `fld`,
  and it will return a new record

* The second element is the current value of the field


# Why the coupling?

Why does a lens give us both the value of a field and a function for
setting a new value of that field?

* Suppose that computing the path to the right place in the record for
  the getter is expensive.
  
* This representation allows the setter to reuse that computation.

We can also reduce the number of laws that a lens must obey from 3 to
2 (but that's beyond our scope).


# The get operator

Here is our getter:

~~~~ {.haskell}
(^.) :: rec -> Lens rec fld -> fld
a ^. (Lens f) = pos (f a)
infixr 9 ^.

-- internal
pos :: Store fld rec -> fld
pos (Store _ s) = s
~~~~


# The set operator

And here is our setter:

~~~~ {.haskell}
(^=) :: Lens rec fld -> fld -> rec -> rec
(Lens f) ^= b = peek b . f
infixr 4 ^=

-- internal
peek :: fld -> Store fld rec -> rec
peek s (Store g _) = g s
~~~~


# Constructing a lens

Given a getter and a setter, we can build a lens:

~~~~ {.haskell}
lens :: (rec -> fld) -> (fld -> rec -> rec) -> Lens rec fld
lens get set = Lens $ \a -> Store (\b -> set b a) (get a)
~~~~

Alternatively, we can construct a lens from an *isomorphism* between
record and field types:

~~~~ {.haskell}
iso :: (rec -> fld) -> (fld -> rec) -> Lens rec fld
iso f g = Lens (Store g . f)
~~~~


# A lens for points

Consider our venerable `Point` type:

~~~~ {.haskell}
data Point = Point {
      ptX :: Int
    , ptY :: Int
    } deriving (Show)
~~~~

We need to define two lenses for this type, one to focus on the `x`
coordinate, and another for `y`:

~~~~ {.haskell}
x, y :: Lens Point Int
x = lens ptX (\x pt -> pt {ptX = x})
y = lens ptY (\y pt -> pt {ptY = y})
~~~~


# Using our lens on points

The getter:

~~~~ {.haskell}
>> let pt = Point 1 1
>> pt ^. x
1
~~~~

The setter:

~~~~ {.haskell}
>> (x ^= 2) pt
Point {ptX = 2, ptY = 1}
~~~~


# Revisiting nested data

Let's define a line type, with lenses for its beginning and end
points:

~~~~ {.haskell}
data Line = Line {
      lnBeg :: Point
    , lnEnd :: Point
    } deriving (Show)

beg, end :: Lens Line Point
beg = lens lnBeg (\b l -> l {lnBeg = b})
end = lens lnEnd (\e l -> l {lnEnd = e})
~~~~

Suppose we want to access the `x` coordinate of the end of the line.

Using normal Haskell machinery, we know we can just use composition:

~~~~ {.haskell}
>> let l = Line (Point 1 2) (Point 3 4)
>> (ptX . lnEnd) l
3
~~~~


# Function composition: not gnar enough

By now, we are familiar with (and love) function composition:

~~~~ {.haskell}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
~~~~

However, we can make composition more abstract:

~~~~ {.haskell}
import Prelude hiding (id, (.))

class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c
~~~~

Now we can recast function composition as just an instance of this
more general `Category` class:

~~~~ {.haskell}
instance Category (->) where
    id a = a
    f . g = \x -> f (g x)
~~~~

# Category? Composition? Abstraction? Huh?

We care about the `Category` class because it turns out we can compose
lenses!

~~~~ {.haskell}
import Control.Category

instance Category Lens where
    id = Lens (Store id)

    Lens f . Lens g = Lens $ \a -> case g a of
      Store wba b -> case f b of
	Store wcb c -> Store (wba . wcb) c
~~~~

How do we do this in practice?

Just as we compose two functions to get another function, when we
compose two lenses, we get another lens.


# Composition of lenses

Access a nested field:

~~~~ {.haskell}
>> let l = Line (Point 1 2) (Point 3 4)
>> l ^. (x . beg)
~~~~

Modify a nested field:

~~~~ {.haskell}
>> ((y . end) ^= 7) l
Line {lnBeg = Point {ptX = 1, ptY = 2},
      lnEnd = Point {ptX = 3, ptY = 7}}
~~~~


# A map as a lens

Algebraic data types are not the only place we can use lenses.

They're just as applicable to container types, for instance:

~~~~ {.haskell}
import qualified Data.Map as Map
import Data.Map (Map)

mapLens :: (Ord k) => k -> Lens (Map k v) (Maybe v)
mapLens k = Lens $ \m ->
            let set Nothing  = Map.delete k m
                set (Just v) = Map.insert k v m
                get          = Map.lookup k m
            in Store set get
~~~~


# It's all about focus

We now know how the "algebraic" got into "algebraic data type", and
why (and how) we'd want to focus on an element within a type.

What's next?


# Lenses and triples

Suppose we have this type.

~~~~ {.haskell}
(Int,Int,Int)
~~~~

How many lenses must we define in order to be able to work with all of
its fields?


# Values in a triple

In our type:

~~~~ {.haskell}
(Int,Int,Int)
~~~~

We can create the following number of values:

* $\text{Int} \times \text{Int} \times \text{Int}$

Which of course we can shorten as $\text{Int}^3$ (raised to the 3rd
power).

Suppose we want to update the first field (using a lens, manual
update, or whatever - the mechanism doesn't matter).

Let's poke a hole in that field:

~~~~ {.haskell}
(_,Int,Int)
~~~~

Clearly we're now representing just $\text{Int} \times \text{Int}$ (or
$\text{Int}^2$) values, because we no longer care what value used to
be present in the first field.


# Poking more holes

There are in fact three different ways we could poke holes in our
triple:

~~~~ {.haskell}
(_,Int,Int)
(Int,_,Int)
(Int,Int,_)
~~~~

And each one can express $\text{Int}^2$ values, for a total of:

* $3 \times \text{Int}^2$


# Getting a little more abstract

Let's parameterise our triple, so we no longer know or care what the
type in each field is:

~~~~ {.haskell}
(x,x,x)
~~~~

This can hold $x^3$ values.

When we poke holes in each field, we find that the total number of
values expressible is:

* $3x^2$

This ought to remind you of differential calculus.

Isn't that remarkable?


# Lists - again?

We're so very familiar with the list type by now.

~~~~ {.haskell}
data List x = Null
            | Cons x (List x)
~~~~

From the symbolic shenanigans we saw earlier, let's compute $n(x)$, the
number of values expressible in a list of type `x`:

$n(x) = 1 + x \times n(x)$

Just as the list is a recursive data type, this is a recurrence
relation.

Let's perform a symbolic differentiation on this expression:

$n'(x) = 0 + 1 \times n(x) + x \times n'(x)$

(The last part is from the
[Leibniz rule](http://en.wikipedia.org/wiki/General_Leibniz_rule).)


# More algebraic crunching

We now have:

$n'(x) = 0 + 1 \times n(x) + x \times n'(x)$

Or more simply:

$n'(x) = n(x) + x \times n'(x)$

Rearranging:

$n'(x)(1-x) = n(x)$

And again:

$n'(x) = n(x)/(1-x)$

And finally:

$n'(x) = n(x)^2$

In other words, the derivative of a list is the *product* of two lists.


# Wow! But what does this mean?

It's quite amazing that symbolic differentiation works on recursive
data types.  This discovery was made by
[McBride](http://strictlypositive.org/diff.pdf).

But what can we do with this knowledge?

Recall our earlier phrasing of "poking a hole" in a triple.  Clearly
there's a correspondence between "poking a hole" and modifying data.

We can use these ideas to both modify a list and move around in it.


# Introducing the zipper

Here is the derivative of a list, expressed as a data type:

~~~~ {.haskell}
data Zipper a = Zipper [a] a [a]
              deriving (Show)
~~~~

The unadorned `a` in the middle is our current focus point.  (It's not
required, just a detail of this particular implementation.)

In a regular list, we can only move in one direction: from the head to
the tail.


# From a list to a zipper

This function constructs a zipper from a list:

~~~~ {.haskell}
fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs
fromList _      = error "empty!"
~~~~


# List-like iteration

Here's iteration in the normal "towards the tail" direction:

~~~~ {.haskell}
next :: Zipper a -> Zipper a
next (Zipper ys y (x:xs)) = Zipper (y:ys) x xs
next z                    = z
~~~~

Notice that we save "where we've been" in our other list.  This is
critically important.


# Going backwards

Since we have saved where we've been in the list, we can step back
there again!

~~~~ {.haskell}
prev :: Zipper a -> Zipper a
prev (Zipper (y:ys) x xs) = Zipper ys y (x:xs)
prev z                    = z
~~~~

We can use the fact that we can pattern match against nearby elements
on *both sides* of our current focus to perform useful operations that
need local context, e.g. sliding window algorithms, convolutions, etc.


# Conversion back to a list

What should this function look like?


# More general zippers

The ideas of differentiating data structures and zippers can be
generalized to other recursive data structures, e.g. trees.
