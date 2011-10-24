% Monads and more

# Remember the Maybe type

~~~~ {.haskell}
data Maybe a = Nothing
             | Just a
~~~~


# We know that Maybe is a functor

~~~~ {.haskell}
data Maybe a = Nothing
             | Just a

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)
~~~~


# What does fmap actually mean?

We have a polymorphic type `f`, and `fmap` gives us the ability to:

* Liberate a pure value from the type constructor that refers to it

* Call a function on it, which could return a result of a different
  type
  
* Have the type constructor refer to the type of the result


# So what's f?

That polymorphic type `f` was daunting to me when I was learning
Haskell.

The easiest easy to *begin* to think of `f` is as a "container".

Here is the most basic of containers:

~~~~ {.haskell}
newtype Container a = Container a

instance Functor Container where
    fmap f (Container a) = Container (f a)
~~~~

We can't get any simpler than this, since (being a `newtype`) it
doesn't have a runtime representation at all.


# More containers

~~~~ {.haskell}
instance Functor Container where
    fmap f (Container a) = Container (f a)

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)

instance Functor [] where
    fmap = map
~~~~

Having seen these instances, we can now state with some confidence:

* For a container type, `fmap` replaces every element of the container
  with the result of applying `f` to that element.
  
* It leaves the *structure* of the container unchanged.

In other words, `fmap` will not turn a `Just` into a `Nothing`, or a
3-element list into an empty list, or the like.


# Is that it?

As useful as this intuitive picture is, it's actually not general
enough.

We'd be making a mistake if we thought we had the whole story now,
because the truth is far richer (and stranger).


# Functions

Let's poke about in `ghci`:

~~~~ {.haskell}
>> :type not
not :: Bool -> Bool
~~~~

Remember that the `->` symbol is not magic: it's just a type
constructor.

Using a notational shortcut, we could rewrite the type above as:

~~~~ {.haskell}
(->) Bool Bool
~~~~

If we get rid of the concrete types and replace them with polymorphic
placeholders, we can write a type signature like this:

~~~~ {.haskell}
(->) a b
~~~~


# More fun with functions

Okay, so we know that this is a function type:

~~~~ {.haskell}
(->)
~~~~

And this is a function that accepts an argument of some type `a`, and
gives a result of some other type `b`:

~~~~ {.haskell}
(->) a b
~~~~

So then what's this?

~~~~ {.haskell}
(->) a
~~~~


# Isn't that suggestive?

This type, being a function that accepts an argument of type `a`, is
polymorphic. (Why?)

~~~~ {.haskell}
((->) a)
~~~~

Which suggests that even though it's surely not a container type, we
could write a `Functor` instance.

~~~~ {.haskell}
instance Functor ((->) a) where
    fmap f {- ...what? -}
~~~~


# Stop! Hammer time!

On the whiteboard, let's puzzle through what the `Functor` instance
ought to look like.

~~~~ {.haskell}
instance Functor ((->) a) where
    fmap f {- ...what? -}
~~~~


# I hope you haven't peeked ahead!

Because here's that definition we were scrawling on the whiteboard.

~~~~ {.haskell}
instance Functor ((->) a) where
    fmap f g = \x -> f (g x)
~~~~

Which we can simplify to:

~~~~ {.haskell}
instance Functor ((->) a) where
    fmap f g = f . g
~~~~

And again:

~~~~ {.haskell}
instance Functor ((->) a) where
    fmap = (.)
~~~~


# Wow. Wow!

Function application is somehow a functor?

I know, right!?

Let's play with that in `ghci` a bit.


# So really, what's a functor?

A functor (in Haskell) is simply a pair of things:

* A polymorphic type

* A definition for `fmap`

The instance has to obey two simple laws:

~~~~ {.haskell}
fmap id      == id
fmap (f . g) == fmap f . fmap g
~~~~

As usual with laws, it's up to you as a coder to satisfy the laws with
your `Functor` instances.


# The next step

In the `Control.Applicative` package we'll find another extremely
useful typeclass:

~~~~ {.haskell}
class Functor f => Applicative f where

    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b
~~~~

The `Applicative` class builds on functors to add two very useful new
behaviours.

* The `pure` operator lifts a normal value into the type.

* The `<*>` operator sequences function application through the type.


# Lifting again

If those definitions feel slippery, remember the signature for `fmap`:

~~~~ {.haskell}
fmap :: (a -> b) -> f a -> f b
~~~~

The only difference between `fmap` and `<*>` is that `<*>` starts off
with the function wrapped in the type `f`.

~~~~ {.haskell}
(<*>) :: f (a -> b) -> f a -> f b
~~~~

This is easiest to follow with a concrete example.

~~~~ {.haskell}
instance Applicative Maybe where
    pure = Just

    Just f <*> Just a = Just (f a)
    _      <*> _      = Nothing
~~~~


# Thinking about constraints

Which of these questions is easier to answer?

* "I'm thinking of a number. It's less than 5."

* "I'm thinking of a number. It's less than 5. It's greater than
  zero."

By adding more information, we've constrained the possible numbers
available to be guessed.

We can loosely carry this line of thinking over to typeclasses:

* The more functions a typeclass has, the more constraints there are
  on the types that can possibly implement it.

* The more laws an instance must satisfy, the smaller the number of
  types that can satisfy them.


# Function application as an applicative functor

To keep those brains nice and groovy, let's look at how function
application lives as an `Applicative`.

~~~~ {.haskell}
instance Applicative ((->) a) where
    pure _  = \x -> x

    f <*> g = \x -> f x (g x)
~~~~

# Functor vs Applicative

The `Functor` class has one method and two laws.

The `Applicative` class adds two methods and four laws (the laws are
simple and intuitive, but I'm not going to describe them).

By appeal to my prior handwaving, the added richness of `Applicative`
comes at a cost: 

* There exist many more types that we can turn into functors than into
  applicatives.

That richness is appealing though: we'll take it when we can. (But I'm
not going to explain why just yet.)


# Functor vs Applicative, revisited

An applicative is once again a triple of things:

* A polymorphic type that we know to be a functor

* A definition for `pure`

* A definition for `(<*>)`

And the definitions have to satisfy the aforementioned four laws
(which you can
[look up for yourself](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#t:Applicative)).


# And on to monads

Monads represent another step up the ladder of richness from
applicative functors.

This is, of course, thanks to the bind operator.

~~~~ {.haskell}
(>>=) :: Monad m => m a -> (a -> m b) -> m b
~~~~

We've already used this operator aplenty, but even so, we lacked the
background to discuss why it matters.


# What we gain with a bind operator

Here's a piece of code that we simply can't express using only the
`Functor` or `Applicative` machinery:

~~~~ {.haskell}
oddEnough act = do
  v <- act
  if odd v
    then fail "too odd!"
    else return v
~~~~

Neither `fmap` nor `<*>` lets us "change the shape" of the outcome of
a series of operations, but `>>=` *does*.

To make that more concrete:

* Any number of compositions of `fmap` over a *k*-element
  list will always give back a *k*-element list.
  
* The `>>=` operator lets us inspect an intermediate result and thus
  change what the final result will be, and do so without knowing how
  the `Monad` instance is constructed.


# Some examples

Here's a standard function that takes a predicate expression and an
action, and executes the action only if the predicate succeeds:

~~~~ {.haskell}
when :: (Monad m) => Bool -> m () -> m ()
when p act = if p then act else return ()
~~~~

Notice that we've defined a very handy control flow function that will
work in *all* monads.

Suppose we want to perform an action that returns a success/fail
indication, and use that result to determine whether to perform a
*second* action.

~~~~ {.haskell}
whenM :: (Monad m) => m Bool -> m () -> m ()
~~~~

Let's write out a body for this function.


# Tantalizing hints

Here's a function from `Control.Monad` that we've seen before:

~~~~ {.haskell}
liftM :: Monad m   => (a -> r) -> m a -> m r
~~~~

It bears a striking resemblance to this:

~~~~ {.haskell}
fmap  :: Functor f => (a -> b) -> f a -> f b
~~~~

And here's a function we just met:

~~~~ {.haskell}
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
~~~~

Which looks very similar to this `Control.Monad` combinator:

~~~~ {.haskell}
ap    :: Monad m       => m (a -> b) -> m a -> m b
~~~~


# A little history

We've seen that `Applicative` is defined to be a subclass of
`Functor`:

~~~~ {.haskell}
class Functor f => Applicative f {- ... -}
~~~~

Shouldn't one of these hold, too?

~~~~ {.haskell}
class Functor m => Monad m {- ... -}

class Applicative m => Monad m {- ... -}
~~~~

"Yes" in principle, but for historical reasons, "no".

Monads and functors were introduced to Haskell around the same time,
and I'm not sure the possible relationship between them was recognized
at the time.

Applicative functors
[arrived on the scene much later](http://www.soi.city.ac.uk/~ross/papers/Applicative.html).
By the time a possible resolution to the tangle was identified, there
was too much code "in the wild" to change things.


# Function application as a monad

Continuing our theme that just like functors and applicatives, monads
are not limited to container types:

~~~~ {.haskell}
instance Monad ((->) r) where
    -- same as the pure method of Applicative
    return _ = \x -> x

    f >>= g  = \x -> g (f x) x
~~~~


# Parsing

Suppose we want to parse part of a string.

We need to consume some - but probably not all - of the input, and
return a result.  Let's return the remainder of the input that we
haven't consumed, so someone else can deal with it.

~~~~ {.haskell}
parse :: String -> (a, String)
~~~~


# Purely functional random numbers

Let's briefly get back to some material I didn't have time to cover a
few weeks ago.

Haskell supplies a `random` package that we can use in a purely
functional setting.

~~~~ {.haskell}
class Random a where
    random :: RandomGen g => g -> (a, g)

class RandomGen g where
    next   :: g -> (Int, g)
    split  :: g -> (g, g)
~~~~


# "Modifying" state

Notice the similarities between these types:

~~~~ {.haskell}
random :: RandomGen g => g      -> (a, g)
parse  ::                String -> (a, String)
~~~~

In each case, we emulate "state update" by returning a new state.


# Yuck!

From that earlier lecture's unseen slides, recall that threading
through all those updates is a pain:

~~~~ {.haskell}
guess :: (RandomGen g) => (Double,g) -> (Double,g)
guess (_,g) = (z, g'')
    where z        = x^2 + y^2
          (x, g')  = random g
          (y, g'') = random g'
~~~~

It would be really easy to reuse a piece of state by accident, and
this is a very simple function!


# A new look at state transformation

Here's our most general setting for those state transformation
functions:

~~~~ {.haskell}
s -> (a,s)
~~~~

If we pay no attention to the `s` parameter, we have one of the
crucial criteria for being a `Functor`, `Applicative`, or `Monad`:

* A fully polymorphic type

What about the rest?


# A little protection

It would actually be a bad thing if we were to declare this type to be
a `Functor`:

~~~~ {.haskell}
s -> (a,s)
~~~~

Why?  It would overlap with the `Functor` instance for `((->) a)`.

To avoid the potential for overlapping instances, we wrap up our state
transformation type in a `newtype`.

~~~~ {.haskell}
newtype State s a = State {
      runState :: s -> (a,s)
    }
~~~~


# A Functor instance

~~~~ {.haskell}
instance Functor (State s) where
    fmap f (State action) = State $ \origState ->
      let (a, newState) = action origState
      in (f a, newState)
~~~~

The nice thing about our state transformer is that it works over *all*
states. Some examples include:

* The current state of a PRNG

* The not-yet-consumed input for a parser


# And a Monad instance

~~~~ {.haskell}
instance Monad (State s) where
    return a = State $ \s -> (a, s)

    State act >>= k = State $ \s ->
      let (a, s') = act s
      in runState (k a) s'
~~~~

The bind operator simply passes the result of the first operation to
the second.


# Manipulating state directly

We can retrieve the current state by copying it into the result field
of our pair.

~~~~ {.haskell}
get :: State s s
get = State $ \s -> (s, s)
~~~~

If we want to replace the current state with a modified version,
that's equally simple.

~~~~ {.haskell}
put :: s -> State s ()
put s = State $ \_ -> ((), s)
~~~~


# Before

Recall this function:

~~~~ {.haskell}
guess :: (RandomGen g) => (Double,g) -> (Double,g)
guess (_,g) = (z, g'')
    where z        = x^2 + y^2
          (x, g')  = random g
          (y, g'') = random g'
~~~~


# With a little help

~~~~ {.haskell}
import System.Random
import Control.Monad.State

modify' :: MonadState s m => (s -> (a,s)) -> m a
modify' f = do
  s <- get
  let (a,s') = f s
  put s'
  return a
~~~~

The `MonadState` class takes the `State`-specific methods, and makes
them available for other monads to implement:

~~~~ {.haskell}
class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
~~~~


# Functional dependencies

Who noticed the vertical bar and arrow?

~~~~ {.haskell}
class (Monad m) => MonadState s m | m -> s {- ... -}
~~~~

This is called a *functional dependency*.  Fundeps are used to make
type checking of multi-parameter type classes tractable.

This fundep tells the type checker (and us) that the type of the state
parameter `s` can be determined from the type of the monad parameter
`m`.

How does this work?

~~~~ {.haskell}
instance MonadState s (State s) where
    get   = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)
~~~~

Here, we're saying that the type `State s` is our monad, and the
fundep ties its `s` parameter to the `s` parameter of the `MonadState`
class.


# A new guesser

Here's a rewrite of our earlier `guess` function to use the `modify'`
function that we just wrote:

~~~~ {.haskell}
guess :: RandomGen g => State g Double
guess = do
  a <- modify' random
  b <- modify' random
  return (a*a + b*b)
~~~~

Notice that we've managed to completely hide the state of the PRNG!


# Why functional dependencies?

Suppose we were to write a simpler multi-parameter type class, without
the fundep:

~~~~ {.haskell}
class (Monad m) => MonadState s m {- ... -}
~~~~

And suppose we were to try to typecheck these type signatures:

~~~~ {.haskell}
modify' :: MonadState s m => (s -> (a,s)) -> m a

guess :: RandomGen g => State g Double
~~~~

Without the fundep, the compiler would choke on these, because it has
no information to tell it that the `g` parameter to `State` is related
to the `s` parameter to `MonadState`.
