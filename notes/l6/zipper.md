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
