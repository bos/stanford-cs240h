
# CS240h: Functional systems in Haskell

* I'm David Mazi&egrave;res
    * Spent most of my career working on OSes, Systems, and Security
    * Previously used C++ and C, but started using Haskell a couple of
      years ago
    * Course partly inspired by my experience learning Haskell

* Also teaching this class: Bryan O'Sullivan
    * Has implemented many key Haskell libraries in widespread use
      today
    * Co-wrote [*Real World Haskell*][RWH], a great non-theoretical
      intro book
    * Also plenty of systems experience (e.g., Linux early userspace
      code)
    
* Course assistant: David Terei
    * Implemented LLVM backend and type-safe extensions that ship
      with GHC Haskell compiler
    * Currently using Haskell for security research here at Stanford

# Why Haskell?

* Haskell's expressive power can improve productivity
    * Small language core provides big flexibility
    * Code can be very concise, speeding development
    * Get best of both worlds from compiled and interpreted languages

* Haskell makes code easier to understand and maintain
    * Can dive into complex libraries and understand *what* the code
      is doing<br> (*why* may be a different story, but conciseness
      leaves room for comments...)

* Haskell can increase the robustness of systems
    * Strong typing catches many bugs at compile time
    * Functional code permits better testing methodologies
    * Can parallelize non-concurrent code without changing semantics
    * Concurrent programming abstractions resistant to data races

* Haskell lets you realize new types of functionality (DIFC, STM, ...)

# Why take CS240h?

* Learn to build systems in Haskell with reduced upfront cost
    * Historically, Haskell was a vehicle for language research.<br>
      The history is reflected in how the language is usually taught
    * CS240h will present the language more from a systems perspective

* Learn new, surprising, and effective programming techniques
    * There are more than enough to fill a 10-week quarter
    * Often only documented in more theoretical papers

* **You enjoy programming**
    * With Haskell, you will think about programming in new ways

* You sometimes get frustrated with other languages
    * Maybe you've wanted to design a new language, or tend to
      "max-out" existing language features (macros, templates,
      overloading, etc.)
    * Things that require changes to most languages can be done in a
      library with Haskell

# Administrivia

* We assume some of you may have toyed with Haskell, others not
* First week cover Haskell basics
    * If you haven't used Haskell, you should supplement by reading
      parts of [Bryan's book][RWH] and/or on-line tutorials (such as
      <http://www.haskell.org/tutorial/>).
    * If you have used Haskell, you may still learn some things from
      these lectures
      
* Rest of term covers more advanced techniques
* Final grade will be based on several factors
    1. Two small warm-up solo programming exercises
    1. A large final project & presentation
    1. Class attendance and participation

# Final project

* Final project is most important component of grade
* Consists of a Haskell-related project of your choice
    * Form project team of 1-3 people
    * Meet with one of the instructors to discuss project
    * Complete and evaluate project and turn in short paper
    * Final exam will be mini-conference where you present your
      work<br>**(Make sure you are here for exam December 15)**

* Class home page has list of
  [suggested projects](http://cs240h.scs.stanford.edu/labs/project.html)
  (we will add more)
* We encourage overlap of CS240h project with your research
    * The programming techniques you learn in CS240h are likely
      orthogonal to whatever research you are doing
* We are okay with CS240h project also serving as another class
  project,<br>**provided the other instructor and all teammates (from
  both classes) approve**


# Getting started with Haskell

* Install the [Haskell Platform][Platform], which includes the
  [GHC][GHCdoc] compiler.
* Create a file called `hello.hs` with the following contents:

    ~~~ {.haskell}
    main = putStrLn "Hello, world!"
    ~~~

* Compile your program to a native executable like this:

    ~~~
    $ ghc --make hello
    [1 of 1] Compiling Main             ( hello.hs, hello.o )
    Linking hello ...
    $ ./hello
    Hello, world!
    ~~~

* Or run it in the [GHCI interpreter][GHCI] like this:

    ~~~
    $ ghci hello.hs 
    GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
    ...
    Ok, modules loaded: Main.
    *Main> main
    Hello, world!
    *Main> 
    ~~~


# Bindings

* Haskell uses the `=` sign to declare *bindings*:

    ~~~ {.haskell}
    x = 2                   -- Two hyphens introduce a comment
    y = 3                   --    ...that continues to end of line.
    main = let z = x + y    -- let introduces local bindings
           in print z       -- program will print 5
    ~~~

    * Bound names cannot start with upper-case letters
    * Bindings are separated by "`;`", which is usually auto-inserted
      by a
      [layout](http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7)
      rule

* A binding may declare a *function* of one or more arguments
    * Function and arguments are separated by spaces (when defining or
      invoking them)

    ~~~ {.haskell}
    add arg1 arg2 = arg1 + arg2   -- defines function add
    five = add 2 3                -- invokes function add
    ~~~

* Parentheses can wrap compound expressions, must do so for arguments

    ~~~ {.haskell}
    bad = print add 2 3     -- error! (print should have only 1 argument)
    ~~~

    ~~~ {.haskell}
    main = print (add 2 3)  -- ok, calls print with 1 argument, 5
    ~~~

# Haskell is a *pure* functional language

* Unlike variables in imperative languages, Haskell bindings are
    * *immutable* - can only bind a symbol once in a give scope<br>
      (We still call bound symbols "variables" though)

    ~~~ {.haskell}
    x = 5
    x = 6                      -- error, cannot re-bind x
    ~~~

    * *order-independent* - order of bindings in source code does not
       matter
    * *lazy* - definitions of symbols are evaluated only when needed

    ~~~ {.haskell}
    safeDiv x y =
        let q = div x y        -- safe as q never evaluated if y == 0
        in if y == 0 then 0 else q
    main = print (safeDiv 1 0) -- prints 0
    ~~~

    * *recursive* - the bound symbol is in scope within its own
       definition

    ~~~ {.haskell}
    x = 5                 -- this x is not used in main

    main = let x = x + 1  -- introduces new x, defined in terms of itself
           in print x     -- program "diverges" (i.e., loops forever)
    ~~~

# How to program without mutable variables?

* In C, we use mutable variables to create loops:

	~~~ {.c}
	long factorial (int n)
	{
	  long result = 1;
	  while (n > 1)
	    result *= n--;
	  return result;
	}
	~~~

* In Haskell, can use recursion to "re-bind" argument symbols in new
  scope

	~~~ {.haskell}
	factorial n = if n > 1
	              then n * factorial (n-1)
	              else 1
	~~~

    * Recursion often fills a similar need to mutable variables
    * But the above Haskell factorial is inferior to the C one--why?


# Tail recursion

* Each recursive call may require a stack frame
    * This Haskell code requires `n` stack frames

        ~~~ {.haskell}
        factorial n = if n > 1 then n * factorial (n-1) else 1
        ~~~

    * By contrast, our C factorial ran in constant space
* Fortunately, Haskell supports optimized *tail recursion*
    * A function is tail recursive if it ends with a call to itself
    * Unfortunately, `factorial n` multiplies by `n` *after*
      evaluating `factorial (n-1)`
* Idea: use *accumulator* argument to make calls tail recursive

    ~~~ {.haskell}
    factorial n = let loop acc n' = if n' > 1
                                    then loop (acc * n') (n' - 1)
                                    else acc
                  in loop 1 n
    ~~~ 

    * Here `loop` is tail recursive, compiles to an actual loop

# Guards and `where` clauses

* *Guards* let you shorten function declarations:

    ~~~ {.haskell}
    factorial n = let loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
                                  | otherwise = acc
                  in loop 1 n
    ~~~

    * "`|`" symbol introduces a guard
    * Guards are evaluated top to bottom; the first `True` guard wins
    * The system Prelude (standard library) defines `otherwise = True`

* Bindings can also end with `where` clauses--like inverted `let`

    ~~~ {.haskell}
    factorial n = loop 1 n
        where loop acc n' | n' > 1    = loop (acc * n') (n' - 1)
                          | otherwise = acc
    ~~~

    * Unlike `let`, a `where` clause scopes over multiple guarded
      definitions
    * This is convenient for binding variables to use in guards


# Tip: variable names

* Inner functions (e.g., `loop`) often have arguments related to
  outer function
    * It is legal to shadow bindings and re-use variable names, but
      the compiler will warn you
    * Typical practice is to add `'` (prime) to the inner-function's
      argument
    * Haskell accepts the `'` character in variables, except as first
      character
* Personally, I find this practice a bit error-prone
    * While learning Haskell, I repeatedly made the error of dropping
      primes, e.g.:

    ~~~ {.haskell}
    factorial n = loop 1 n
        where loop acc n' | n' > 1    = loop (acc * n) (n' - 1) -- bug
                          | otherwise = acc
    ~~~

    * You can avoid the problem by using the longer symbol name for
      the outer function

    ~~~ {.haskell}
    factorial n0 = loop 1 n0
        where loop acc n | n > 1     = loop (acc * n) (n - 1)
                         | otherwise = acc
    ~~~

    * Here accidentally typing "`factorial n0 = loop 1 n`" causes
      compile error

# Every expression and binding has a type

* Some basic types:
    * `Bool` - either `True` or `False`
    * `Char` - a unicode code point (i.e., a character)
    * `Int` - fixed-size integer
    * `Integer` - an arbitrary-size integer
    * `Double` - an IEEE double-precision floating-point number
    * *type1* `->` *type2* - a function from *type1* to *type2*
    * `(`*type1*`,` *type2*`,` ...`,` *typeN*`)` - a tuple
    * `()` - a zero-tuple, pronounced *unit* (kind of like `void` in
      C); there only one value of this type, also written `()`
* You can declare the type of a symbol or expression with `::`

    ~~~ {.haskell}
    x :: Integer
    x = (1 :: Integer) + (1 :: Integer) :: Integer
    ~~~

    * `::` has lower precedence than any function operators (including
      `+`)


# More on types

* Function application happens one argument at a time
  (a.k.a. "*currying*")

    ~~~ {.haskell}
    add :: Integer -> (Integer -> Integer)
    add arg1 arg2 = arg1 + arg2
    ~~~

    * So `add 2 3` is equivalent to `(add 2) 3`
    * `(add 2)` takes 3 returns 5, so `(add 2) has type Integer -> Integer`
    * `->` associates to the right, so parens usually omitted in
      multi-argument function types:<br>
      `fn ::` *argType1* `->` *argType2* `->` ... `->` *argTypeN* `->`
      *resultType*

* Usually the compiler can infer types
    * You can ask [GHCI][GHCI] to show you inferred types with `:t`

    ~~~
    *Main> :t add
    add :: Integer -> Integer -> Integer
    ~~~

    * Good practice to declare types of top-level bindings
      anyway (compiler warns if missing)

# User-defined data types

* The `data` keyword declares user-defined data types (like `struct`
  in C), E.g.:

    ~~~ {.haskell}
    data PointT = PointC Double Double deriving Show
    ~~~

    * This declaration declares a new type, `PointT`, and constructor,
      `PointC`
    * A value of type `PointT` contains two `Double`s
    * `deriving Show` means you can print the type (helpful in GHCI)

* Note that data types and constructors must start with capital letters
* Types and constructors can use the same name (often do), E.g.:
    
    ~~~ {.haskell}
    data Point = Point Double Double deriving Show
    ~~~ 

* One type can have multiple constructors (like a tagged union):

    ~~~ {.haskell}
    data Point = Cartesian Double Double
               | Polar Double Double
                 deriving Show
    ~~~

    ~~~ {.haskell}
    data Color = Red | Green | Blue | Indigo | Violet deriving Show
    ~~~

# Using data types

* Constructors act like functions producing values of their types

    ~~~ {.haskell}
    data Point = Point Double Double deriving Show
    myPoint :: Point
    myPoint = Point 1.0 1.0
    ~~~

    ~~~ {.haskell}
    data Color = Red | Green | Blue | Indigo | Violet deriving Show
    myColor :: Color
    myColor = Red
    ~~~

* `case` statements & function bindings "de-construct" values with
  *patterns*

    ~~~ {.haskell}
    getX, getMaxCoord :: Point -> Double
    getX point = case point of
                   Point x y -> x
    getMaxCoord (Point x y) | x > y     = x
                            | otherwise = y
    ~~~

    ~~~ {.haskell}
    isRed :: Color -> Bool
    isRed Red = True        -- Only matches constructor Red
    isRed c   = False       -- Lower-case c just a variable
    ~~~

# Parameterized types

* Types can have parameters sort of the way functions do
    * Type parameters start with lower-case letters
    * Some examples from the standard Prelude

    ~~~ {.haskell}
    data Maybe a = Just a
                 | Nothing
    ~~~

    ~~~ {.haskell}
    data Either a b = Left a
                    | Right b
    ~~~

* You can see these at work in GHCI:

    ~~~
    Prelude> :t Just True
    Just True :: Maybe Bool
    Prelude> :t Left True
    Left True :: Either Bool b   
    ~~~

* Notice the type of `Left True` contains a type variable, `b`
    * Expression `Left True` can be of type `Either Bool b` for any
      type `b`
    * This is an example of a feature called *parametric polymorphism*

# More deconstruction tips

* Special variable "`_`" can be bound but not used
    * Use it when you don't care about a value:

    ~~~ {.haskell}
    isJust :: Maybe a -> Bool      -- note parametric polymorphism
    isJust (Just _) = True
    isJust Nothing  = False
    ~~~

    ~~~ {.haskell}
    isRed Red = True
    isRed _   = False              -- we don't need the non-red value
    ~~~

    * Compiler warns if a bound variable not used; `_` avoids this

* You can deconstruct types and bind variables within guards, E.g.:

    ~~~ {.haskell}
    addMaybes mx my | Just x <- mx, Just y <- my = Just (x + y)
    addMaybes _ _                                = Nothing
    ~~~

    though often there is a simpler way
    
    ~~~ {.haskell}
    addMaybes (Just x) (Just y) = Just (x + y)
    addMaybes _ _               = Nothing
    ~~~


# Lists

* We could define homogeneous lists with the `data` keyword

    ~~~ {.haskell}
    data List a = Cons a (List a) | Nil

    oneTwoThree = (Cons 1 (Cons 2 (Cons 3 Nil))) :: List Integer
    ~~~

* But Haskell has built-in lists with syntactic sugar
    * Instead of `List Integer`, the type is written `[Integer]`
    * Instead of `Cons`, the constructor is called `:` and is *infix*
    * Instead of `Nil`, the empty list is called `[]`

    ~~~ {.haskell}
    oneTwoThree = 1:2:3:[] :: [Integer]
    ~~~

    * But there are even more convenient syntaxes for the same list:

    ~~~ {.haskell}
    oneTwoThree' = [1, 2, 3]    -- comma-separated elements within brackets
    oneTwoThree'' = [1..3]      -- define list by a range
    ~~~

* A `String` is just a list of `Char`, so `['a', 'b', 'c'] == "abc"`

# Some basic list functions in Prelude

~~~ {.haskell}
head :: [a] -> a
head (x:_) = x
head []    = error "head: empty list"
~~~

~~~ {.haskell}
tail :: [a] -> a             -- all but first element
tail (_:xs) = xs
tail []     = error "tail: empty list"
~~~

~~~ {.haskell}
a ++ b :: [a] -> [a] -> [a]  -- infix operator concatenate lists
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys
~~~

~~~ {.haskell}
length :: [a] -> Int         -- This code is from language spec
length []    =  0            -- GHC implements differently, why?
length (_:l) =  1 + length l
~~~

~~~ {.haskell}
filter :: (a -> Bool) -> [a] -> [a]
filter pred [] = []
filter pred (x:xs)
  | pred x     = x : filter pred xs
  | otherwise  = filter pred xs
~~~

Note function `error :: String -> a` reports assertion failures

# Hoogle

* Let's find the source code for GHC's `length` function?
* [Hoogle] is a search engine just for Haskell functions
    * Go to <http://www.haskell.org/hoogle/>
    * Click on *search plugin*
    * Keyword "`haskell.org`" is too long for me--I change to "`ho`"

* Let's search for length... click on source
    * All those `#` marks are for "unboxed types", which are faster
      but not asymptotically
    * The important point is that `len` is tail recursive

* I use Hoogle all the time, all the time when coding
    * Most of the source code is not hard to understand
    * Length may be a bad starter example just because of unboxed
      types
    * Try examining the code of the functions you are using to
      understand them better

# Example: counting letters

* Here's a function to count lower-case letters in a `String`

    ~~~ {.haskell}
    import Data.Char    -- brings function isLower into scope

    countLowerCase :: String -> Int
    countLowerCase str = length (filter isLower str)
    ~~~

* If we fix `length`, `countLowerCase` might run in constant space
    * Recall Haskell evaluates expressions lazily...  Means in most
      contexts values are interchangeable with function pointers
      (a.k.a. *thunks*)

    * A `String` is a `[Char]`, which is type with two values, a
      *head* and *tail*

    * But until each of the *head* or *tail* are needed, they can be
      stored as function pointers

    * So `length` will causes `filter` to produce `Char`s one at a time

    * `length` does not hold on to characters once counted; 
      can be garbage-collected at will

# Function composition
    
* Here's an even more concise definition

    ~~~ {.haskell}
    countLowerCase :: String -> Int
    countLowerCase = length . filter isLower
    ~~~

* The "`.`" operator provides function composition

    ~~~ {.haskell}
    (f . g) x = f (g x)
    ~~~

    * On previous slide, `countLowerCase`'s argument had name `str`
    * The new version doesn't name the argument, a style called
      *point-free*

* Function composition can be used almost like Unix pipelines

    ~~~ {.haskell}
    process = countLowercase . toPigLatin . extractComments . unCompress
    ~~~

# Lambda abstraction

* Sometimes you want to name the arguments but not the function
* Haskell allows anonymous functions through *lambda abstraction*
    * The notation is `\`*variable(s)* `->` *body* (where `\` is
      pronounced "lambda")

* Example:

    ~~~ {.haskell}
    countLowercaseAndDigits :: String -> Int
    countLowercaseAndDigits = length . filter (\c -> isLower c || isDigit c)
    ~~~

* Lambda abstractions can deconstruct values with patterns, e.g.:

    ~~~ {.haskell}
            ... (\(Right x) -> x) ...
    ~~~

    * But note that guards or multiple bindings are not allowed
    * Patterns must have the right constructor or will get run-time error

# Infix vs. Prefix notation

* We've seen some infix functions & constructors: `+`, `*`, `/`, `.`, `||`,
  `:`
* In fact, any binary function or constructor can be used infix or
  prefix
* For functions and constructors composed of letters, digits, `_`, and
  `'`
    * Prefix is the default: `add 1 2`
    * Putting function in backticks makes it infix: `` 1 `add` 2 ``
* For functions starting with one of `!#$%&*+./<=>?@\^|-~` or
  constructors starting "`:`"
    * Infix is default, Putting functions in parens makes them prefix,
     e.g., `(+) 1 2`
* For tuples, prefix constructors are `(,)`, `(,,)`, `(,,,)`, `(,,,,)`, etc.
* Infix functions can be partially applied in a parenthesized
  *section*

    ~~~ {.haskell}
    stripPunctuation :: String -> String
    stripPunctuation = filter (`notElem` "!#$%&*+./<=>?@\\^|-~:")
    -- Note above string the SECOND argument to notElem ^
    ~~~

# Fixity

* Most operators are just library functions in Haskell
    * Very few operators reserved by language syntax (`..`, `:`, `::`,
      `=`, `\`, `|`, `<-`, `->`, `@`, `~`, `=>`, `--`)
    * You can go crazy and define your own operators
    * Or even use your own definitions instead of system ones
* Define precedence of infix operators with fixity declarations
    * Keywords: `infixl`/`infixr`/`infix` for left/right/no
      associativity
    * Syntax: *infix-keyword* [0-9] *function* [, *function* ...]
    * Allowed wherever a type declaration is allowed
* 0 is lowest allowed fixity precedence, 9 is highest
    * Prefix function application has fixity 10--higher than any infix
      call
    * Lambda abstractions, `else` clauses, and `let`...`in` clauses
      extend as far to the right as possible (meaning they never stop
      at any infix operator, no matter how low precedence)

# Fixity of specific operators

* Here is the fixity of the
  [standard operators](http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820061):

~~~ {.haskell}
infixl 9  !!             -- This is the default when fixity unspecified
infixr 9  .
infixr 8  ^, ^^, ⋆⋆
infixl 7  ⋆, /, `quot`, `rem`, `div`, `mod`  
infixl 6  +, -           -- Unary negation "-" has this fixity, too
infixr 5  ++             -- built-in ":" constructor has this fixity, too
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<  
infixr 0  $, $!, `seq`
~~~

* If you can't remember, use `:i` in [GHCI][GHCI]:

    ~~~
    Prelude> :i &&
    (&&) :: Bool -> Bool -> Bool    -- Defined in GHC.Classes
    infixr 3 &&
    ~~~

    * If GHCI doesn't specify, means default: `infixl 9`

# The "`infixr 0`" operators

* <span style="color:blue">`$`</span> is function application, but
  with lowest precedence

    ~~~~ {.haskell}
    ($) :: (a -> b) -> a -> b
    f $ x = f x
    ~~~~

    * Turns out to be quite useful for avoiding parentheses, E.g.:

    ~~~~ {.haskell}
        putStrLn $ "the value of " ++ key ++ " is " ++ show value
    ~~~~

* <span style="color:blue">`seq :: a -> b -> b`</span> evaluates
  first argument, and second
    * Means when you are done, first argument is a value, not a thunk

    ~~~~ {.haskell}
    main = let q = 1 `div` 0
           in seq q $ putStrLn "Hello world!\n"  -- exception
    ~~~~

    * `seq` has to be built into the compiler

* <span style="color:blue">`$!`</span> combines `$` and `seq`

    ~~~~ {.haskell}
    f $! x  = x `seq` f x
    ~~~~

# Accumulators revisited

* We used an accumulator to avoid `n0` stack frames in `factorial`:

~~~ {.haskell}
factorial n0 = loop 1 n0
    where loop acc n | n > 1     = loop (acc * n) (n - 1)
                     | otherwise = acc
~~~

* Unfortunately, `acc` can contain a chain of thunks `n` long<br>
    * `(((1 * n) * (n - 1)) * (n - 2) ...)` -- Laziness means only
      evaluated when needed
    * GHC is smart enough not to build up thunks, but only when
      optimizing

* Can fix such problems using `$!` or `seq`

~~~ {.haskell}
factorial n0 = loop 1 n0
    where loop acc n | n > 1     = (loop $! acc * n) (n - 1)
                     | otherwise = acc
~~~

~~~ {.haskell}
factorial n0 = loop 1 n0
    where loop acc n | n > 1     = acc `seq` loop (acc * n) (n - 1)
                     | otherwise = acc
~~~

# Hackage and cabal

* [Hackage](http://hackage.haskell.org/packages/archive/pkg-list.html)
  is a large collection of Haskell packages
* [Cabal](http://www.haskell.org/ghc/docs/7.0-latest/html/Cabal/index.html)
  is a tool for browsing hackage and installing packages
    * Cabal comes with the [haskell platform][Platform]
    * Run `cabal update` to create `$HOME/.cabal`, download package database
    * I highly recommend unconmenting and editing these two lines in
      `$HOME/.cabal/config`

        ~~~
	documentation: True
	library-profiling: True
        ~~~

    * May want to add `$HOME/.cabal/bin` to your path
* To install packages for the next examples, run

    ~~~
    cabal install http-enumerator utf8-string tagsoup
    ~~~

    * Installs packages in `$HOME/.cabal`, and records them in
      `$HOME/.ghc`
    * To start fresh, must delete both `$HOME/.cabal` and `$HOME/.ghc`


# Modules and `import` syntax

* Haskell groups top-level bindings into *modules*
    * Default module name is `Main`, as programs start at function
      `main` in `Main`
    * Except for `Main`, a module named *M* must reside in a file
      named *M*`.hs`
    * Module names are capitalized; I use lower-case file names
      for `Main` modules
* Let's add this to the top of our source file

    ~~~~ {.haskell}
    module Main where      -- redundant since Main is the default
    import qualified Data.ByteString.Lazy.UTF8 as L
    import Data.Char
    import Network.HTTP.Enumerator (simpleHttp)
    import System.Environment
    ~~~~

    * Start module with "`module` *name* `where`" or "`module` *name*
      `(`*exported-symbol*[`,` ...]`) where`"
      (non-exported symbols provide modularity)
    * `import` *module* - imports all symbols in *module*
    * `import qualified` *module* `as` *ID* - prefixes imported symbols
      with *ID*`.`
    * `import` *module* `(`*function1*[`,` *function2* ...]`)` -
      imports just the named functions

# `do` notation

* Let's write a program to dump a web page

~~~ {.haskell}
main = do
  (url:_) <- getArgs       -- Sets url to first command-line argument
  page <- simpleHttp url   -- Sets page to contents as a ByteString
  putStr (L.toString page) -- Converts ByteString to String and prints it
~~~

* This task requires some impure (non-functional) actions
    * Extracting command-line args, creating a TCP connection, writing
      to stdout
* A `do` block lets you sequence IO actions.  In a `do` block:
    * <span style="color:blue">*pat* `<-` *action*</span> - binds
      *pat* (variable or constructor pattern) to result of executing
      *action*
    * <span style="color:blue">`let` *pat* `=` *pure-value*</span> -
    binds *pat* to *pure-value* (no "`in` ..." required)
    * <span style="color:blue">*action*</span> - executes *action* and
      discards the result, or returns it if at end of block
* GHCI input is like `do` block (i.e., can use `<-`, need `let` for
  bindings)
* `do`/`let`/`case` won't parse after prefix function (so say
  "`func $ do` ...")

# What are the types of IO actions?

~~~~ {.haskell}
main :: IO ()
getArgs :: IO [String]
simpleHttp :: String -> IO L.ByteString -- (really more polymorphic)
putStr :: String -> IO ()
~~~~

* `IO` is a parameterized type (just as `Maybe` is parameterized)
    * "`IO [String]`" means IO action that produces a
      `[String]` if executed
    * Unlike `Maybe`, we won't use a constructor for `IO`, which is
      somewhat magic
* What if we try to print the first command-line argument as follows?

    ~~~~ {.haskell}
    main = putStr (head getArgs)
    ~~~~

    * Oops, `head` expects type `[String]`, while `getArgs` is an `IO [String]`

* How to de-construct an `IO [String]` to get a `[String]`
    * We can't use `case`, because we don't have a constructor for
      `IO`... Besides, the order and number of deconstructions of
      something like `putStr` matters
    * That's the point of the `<-` operator in `do` blocks!


# Another way to see IO [[Peyton Jones]][Awkward]

~~~ {.haskell}
do page <- simpleHttp url
   putStr (L.toString page)
~~~

<div style="text-align:center">![](io1.svg)</div>

* `simpleHttp` and `putStr` return `IO` *actions* that can change the
  world
    * Pure code can manipulate such actions, but can't actually
      execute them
    * Only the special `main` action is ever executed


# Another way to see IO [[Peyton Jones]][Awkward]

~~~ {.haskell}
do page <- simpleHttp url
   putStr (L.toString page)
~~~

<div style="text-align:center">![](io2.svg)</div>

* The `do` block builds a compound action from other actions
    * It sequences how actions will be applied to the real world
    * When executed, applies `IO a` actions to the world,
      extracting values of type `a`
    * What action to execute next can depend on the value of the
      extracted `a`

# Running `urldump`

~~~~
$ ghc --make urldump
[1 of 1] Compiling Main             ( urldump.hs, urldump.o )
Linking urldump ...
$ ./urldump http://www.scs.stanford.edu/
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
...
~~~~

* What if you want to run it in GHCI?

    ~~~~
$ ghci ./urldump.hs
Prelude Main>
    ~~~~

    * No `*` before `Main` means no access to internal symbols (because
      compiled)

    ~~~~
Prelude Main> :load *urldump.hs
[1 of 1] Compiling Main             ( urldump.hs, interpreted )
Ok, modules loaded: Main.
*Main> withArgs ["http://cs240h.scs.stanford.edu/"] main
    ~~~~

    * Alternate GHCI shortcut:

    ~~~~
Prelude Main> :main "http://cs240h.scs.stanford.edu/"
    ~~~~

# The `return` function

<!-- might need to check out
https://blueprints.launchpad.net/inkscape/+spec/allow-browser-resizing
-->

* Let's combine `simpleHttp` and `L.toString` into one function
<div style="margin:0;text-align:center;">![](simpleHttpStr.svg)</div>

    ~~~~ {.haskell}
    simpleHttpStr :: String -> IO String
    simpleHttpStr url = do
      page <- simpleHttp url
      return (L.toString page)  -- result of do block is last action
    ~~~~

* Note:  **`return` is not control flow statement**, just a function

    ~~~~ {.haskell}
    return :: a -> IO a
    ~~~~

    * Every action in an `IO` do block must have type `IO a` for some
      `a`
    * `L.toString` returns a `String`, use `return` to make an `IO
      String`
    * In a `do` block, "`let x = e`" is like "`x <- return e`" (except
      recursive)


# Point-free IO composition

* Recall point-free function composition with "`.`" (fixity `infixr 9`)
* Function `>>=` (pronounced "bind") allows point-free IO composition

    ~~~~ {.haskell}
    (>>=) :: IO a -> (a -> IO b) -> IO b
    infixl 1 >>=
    ~~~~

* Let's re-write `urldump` in point-free style

    ~~~~ {.haskell}
    main = getArgs >>= simpleHttpStr . head
    ~~~~

    * Note `>>=` composes left-to-right, while `.` goes right-to-left
* `do` blocks are just
  [syntactic sugar](http://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14)
  for calling `>>=`
    * Let's de-sugar our original `main`:

    ~~~~ {.haskell}
    main =
        getArgs >>= \(url:_) ->
        simpleHttp url >>= \page ->
        putStr (L.toString page)
    ~~~~

# Lazy IO

* Some simple file IO functions may be handy for first lab

    ~~~~ {.haskell}
    type FilePath = String -- makes FilePath synonym for String
    getContents :: IO String          -- read all stdin
    readFile :: FilePath -> IO String -- read (whole) file
    writeFile :: FilePath -> String -> IO ()  -- write file
    ~~~~

* E.g., `main = readFile "input" >>= writeFile "output"`
    * Surprisingly, this program does not require unbounded memory
    * Rather, input is read lazily as the list of Characters is
      evaluated
* How lazy IO works
    * A list has two values, the head and the tail, each possibly a
      thunk
    * At some point evaluating thunk actually triggers file IO
    * Function `unsafeInterleaveIO` creates thunks that execute `IO`
      actions
      (c.f. more widely used `unsafePerformIO`, described in
      [[Peyton Jones]][Awkward])
    * Lazy IO is great for scripts, bad for servers; more in Iteratee
      lecture


# More on polymorphism

* We've seen a bunch of polymorphic functions
* Here are some more handy ones from Prelude

~~~~ {.haskell}
id :: a -> a
id x = x
~~~~

~~~~ {.haskell}
const :: a -> b -> a
const a _ = a
~~~~

~~~~ {.haskell}
fst :: (a, b) -> a
fst (a, _) = a
~~~~

~~~~ {.haskell}
snd :: (a, b) -> b
snd (_, b) = b
~~~~

~~~~ {.haskell}
print a = putStrLn (show a)   -- what's the type?  a -> IO ()?
~~~~

~~~~ {.haskell}
show a = ???                  -- how to implement?
~~~~

# Parametric vs. ad hoc polymorphism

* There are actually *two* kinds of polymorphism at work here
* *parametric polymorphism* -- does the same thing for every type
    * E.g., `id :: a -> a`  just passes the value through
    * Works for every possible type
* *ad hoc polymorphism* -- does different things on different types
    * E.g., `1 + 1` and `1.0 + 1.0` compute very different functions
    * E.g., `show` converts value to `String`, depends entirely on input type
    * Only works on types that support it (hence "`deriving Show`" in
      declarations)


# Classes and Instances

* Ad-hoc polymorphic functions are called *methods* and declared with
  *classes*

    ~~~~ {.haskell}
    class MyShow a where
        myShow :: a -> String
    ~~~~

* The actual method for each type is defined in an *instance*
  declaration

    ~~~~ {.haskell}
    data Point = Point Double Double
    instance MyShow Point where
        myShow (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
    ~~~~

    * A class declaration can also include default definitions for
      methods

* What's the type of a function that calls `myShow`?  Ask GHCI:

    ~~~~ {.haskell}
    myPrint x = putStrLn $ myShow x
    ~~~~

    ~~~~
    *Main> :t myPrint
    myPrint :: MyShow a => a -> IO ()
    ~~~~

# The Context of a type declaration

* Type declarations can contain restrictions on type variables
    * Restrictions expressed with "`(`*class* *type-var*, ...`) =>`"
      at start of type, E.g.:

    ~~~~ {.haskell}
    myPrint :: MyShow a => a -> IO ()
    ~~~~

    ~~~~ {.haskell}
    sortAndShow :: (Ord a, MyShow a) => [a] -> String
    ~~~~

    ~~~~ {.haskell}
    elem :: (Eq a) => a -> [a] -> Bool
    elem _ []     = False
    elem x (y:ys) = x==y || elem x ys
    ~~~~

    ~~~~ {.haskell}
    add :: (Num a) => a -> a -> a
    add arg1 arg2 = arg1 + arg2
    ~~~~

* Can think of context as representing hidden *dictionary* arguments
    * When you call `myPrint`, you explicitly give it a value of type
      `a`
    * But also implicitly give it a function pointer for type `a`'s
      `MyShow` instance

# The [Dreaded][DMRWiki] [Monomorphism Restriction][DMR] (DMR)

* Let's say you want to cache result of super-expensive function

    ~~~~ {.haskell}
    superExpensive val = len $ veryExpensive (val :: Int)
        where len [] = 0
              len (x:xs) = 1 + len xs
    cachedResult = superExpensive 5
    ~~~~

    * `cachedResult` will start as thunk, be executed once, then
      contain value

* Let's think about the types

    ~~~~
    *Main> :t superExpensive
    superExpensive :: Num a => Int -> a
    *Main> :t cachedResult
    cachedResult :: Integer
    ~~~~

    * \+ and 0 are overloaded, so `superExpensive` can return any
      `Num` you want
    * Why don't we have `cachedResult :: (Num a) => a`?
    * Recall context restrictions are like hidden arguments... so
      would make `cachedResult` into a function, undermining our
      caching goal!
    * But how is compiler smart enough to save us here?

# The DMR continued

* Answer: in this case, compiler is not actually that smart
    * Heuristic: If it looks like a function, can infer *ad hoc*
      polymorphic types
    * If it looks like anything else, no *ad hoc* polymorphism unless
      explicitly declared
    * *parametric* polymorphic types can always be inferred (no hidden
      arguments)
* What looks like a function?
    * Has to bind a single symbol (`f`), rather than a pattern (`(x,
      y)`, `(Just x)`)
    * Has to have at least one explicit argument (`f x =` ... ok, `f
      =` ... not)
* How are monomorphic types inferred?
    * If bound symbol used elsewhere in module, infer type from use
    * If still ambiguous and type is of class `Num`, try `Integer`
      then `Double` (this sequence can be changed with a
      [`default` declaration][default])
    * If still ambiguous, compilation fails

# The DMR take-away message

* Think of type restrictions as implicit dictionary arguments
    * Compiler won't saddle non-function with implicit arguments
* This code will compile

    ~~~~ {.haskell}
    -- Compiler infers: show1 :: (Show x) => x -> String
    show1 x = show x
    ~~~~

* But neither of these will:

    ~~~~ {.haskell}
    show2 = show
    show3 = \x -> show x
    ~~~~

    * I'd rather you heard it from me than from GHC...

* Relatively easy to work around DMR
    * Add type signatures to functions--a good idea anyway for
      top-level bindings, and sometimes necessary for `let` bindings

        ~~~~ {.haskell}
        -- No problem, compiler knows you want ad hoc polymorphism
        show2 :: (Show x) => x -> String
        show2 = show
        ~~~~

# Superclasses and instance contexts

* One class may require all instances to be members of another
    * Class `Eq` contains '==' and '/=' methods, while
    `Ord` contains `<`, `>=`, `>`, `<=`, etc.
    * It doesn't make sense to have an `Ord` instance not also be an
      `Eq` instance
    * `Ord` declares `Eq` as a superclass, using a context

        ~~~~ {.haskell}
        class Eq a => Ord a where
            (<), (>=), (>), (<=) :: a -> a -> Bool
            a <= b = a == b || a < b -- default methods can use superclasses
            ....
        ~~~~

    * Don't need to write superclass restrictions in contexts--any
      function with an `Ord` dictionary can lookup the `Eq` dictionary
    * Incidentally, can add `deriving (Eq, Ord)` to `data`
      declarations
* Similarly, an instance may require a context
    * E.g., define `myShow` for a list of items whose type is of class
      `MyShow`

    ~~~~ {.haskell}
    instance (MyShow a) => MyShow [a] where
        myShow [] = "[]"
        myShow (x:xs) = myShow x ++ ":" ++ myShow xs
    ~~~~

# Classes of parameterized types

* Can also have classes of parameterized types
* `Functor` is a class for parameterized types onto which you can map
      functions:

    ~~~~ {.haskell}
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
    ~~~~

    * Notice there are no arguments/results of type `f`, rather types
      `f a` and `f b`

* An example of a `Functor` is `Maybe`:

    ~~~~ {.haskell}
    instance Functor Maybe where
        fmap _ Nothing  = Nothing
        fmap f (Just a) = Just (f a)
    ~~~~

    ~~~~
    GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
    Prelude> fmap (+ 1) Nothing
    Nothing
    Prelude> fmap (+ 1) $ Just 2
    Just 3
    ~~~~

# More `Functor`s

* Lists are a `Functor`

    * `[]` can be used as a prefix type ("`[] Int`" means "`[Int]`")
      and can be used to declare instances

    ~~~~ {.haskell}
    map :: (a -> b) -> [a] -> [b]
    map _ []     = []
    map f (x:xs) = f x : map f xs

    instance Functor [] where
        fmap = map
    ~~~~

* `IO` is a `Functor`

    ~~~~ {.haskell}
    instance Functor IO where
        fmap f x = x >>= return . f
    ~~~~

    * So we could have said:

        ~~~~ {.haskell}
	simpleHttpStr url = fmap L.toString $ simpleHttp url
        ~~~~

        or, simpler still:

        ~~~~ {.haskell}
	simpleHttpStr = fmap L.toString . simpleHttp
        ~~~~

# Kinds

* What happens if you try to make an instance of `Functor` for `Int`?

    ~~~~ {.haskell}
    instance Functor Int where         -- compilation error
        fmap _ _ = error "placeholder"
    ~~~~

    * Get `fmap :: (a -> b) -> Int a -> Int b`, but `Int` not
    parameterized
* The compiler must keep track of all the different kinds of types
    * One kind of type (e.g., `Int`, `Double`, `()`) directly
      describes values
    * Another kind of type (`Maybe`, `[]`, `IO`) requires a type
      parameter
    * Yet another kind of type (`Either`, `(,)`), requires *two
      parameters*
    * Parameterized types are sometimes called *type constructors*
* Kinds named using symbols &#x2217; and &#x2192;, much like curried
  functions
    * &#x2217; is the kind of type that represents values (`Int`,
      `Double`, `()`, etc.)
    * &#x2217; &#x2192; &#x2217; is the kind of type with one
      parameter of type &#x2217; (`Maybe`, `IO`, etc.)
    * &#x2217; &#x2192; &#x2217; &#x2192; &#x2217; is a type
      constructor with two arguments of kind &#x2217; (`Either`)
    * In general, *a* &#x2192; *b* means a type constructor that,
      applied to kind *a*, yields kind *b*



# The `Monad` class

* **The entire first two lectures have been working up to this slide**
* `return` and `>>=` are actually methods of a class called `Monad`

~~~~ {.haskell}
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
    fail :: String -> m a   -- called when pattern binding fails
    fail s = error s        -- default is to throw exception

    (>>) :: m a -> m b -> m a
    m >> k = m >>= \_ -> k
~~~~

* This has far-reaching consequences
    * You can use the syntactic sugar of `do` blocks for non-IO
      purposes
    * Many monadic functions are polymorphic in the `Monad`--invent a
      new monad, and you can still use much existing code

# The `Maybe` monad

* System libraries define a `Monad` instance for `Maybe`

    ~~~~ {.haskell}
    instance  Monad Maybe  where
        (Just x) >>= k = k x
        Nothing >>= _  = Nothing
        return = Just
        fail _ = Nothing
    ~~~~

* You can use `Nothing` to indicate failure
    * Might have a bunch of functions to extract fields from data

    ~~~~ {.haskell}
    extractA :: String -> Maybe Int
    extractB :: String -> Maybe String
    ...
    parseForm :: String -> Maybe Form
    parseForm raw = do
        a <- extractA raw
        b <- extractB raw
        ...
        return (Form a b ...)
    ~~~~

    * Threads success/failure state through system as `IO` threaded
      World
    * Since Haskell is lazy, stops computing at first `Nothing`

# Algebraic data types

* Some data types have a large number of fields

    ~~~~ {.haskell}
    -- Argument to createProcess function
    data CreateProcess = CreateProcess CmdSpec (Maybe FilePath)
        (Maybe [(String,String)]) StdStream StdStream StdStream Bool
    ~~~~

    * Quickly gets rather unwieldy

* Algebraic data types let you label fields (like C `struct`s)

    ~~~~ {.haskell}
    data CreateProcess = CreateProcess {
      cmdspec   :: CmdSpec,
      cwd       :: Maybe FilePath,
      env       :: Maybe [(String,String)],
      std_in    :: StdStream,
      std_out   :: StdStream,
      std_err   :: StdStream,
      close_fds :: Bool
    }
    ~~~~

* Let's make an algebraic version of our `Point` class

    ~~~~ {.haskell}
    data Point = Point { xCoord :: Double, yCoord :: Double }
    ~~~~

# Algebraic types - initialization and matching

~~~~ {.haskell}
data Point = Point { xCoord :: Double, yCoord :: Double }
~~~~

* Can initialize an Algebraic type by naming fields

    ~~~~ {.haskell}
    myPoint = Point { xCoord = 1.0, yCoord = 1.0 }
    ~~~~

    * Uninitialized fields get value `undefined` - a thunk that throws
      an exception

* Can also pattern-match on any subset of fields

    ~~~~ {.haskell}
    -- Note the pattern binding assigns the variable on the right of =
    getX Point{ xCoord = x } = x
    ~~~~

    * [*As-patterns*](http://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-590003.17.1)
      are handy to bind a variable and pattern simultaneously (with
      `@`):

        ~~~~ {.haskell}
	getX' p@Point{ xCoord = x }
                | x < 100 = x
                | otherwise = error $ show p ++ " out of range"
        ~~~~

        ~~~~ {.haskell}
        -- Also works with non-algebraic patterns
	getX' p@(Point x _) = ...
	processString s@('$':_) = ...
	processString s         = ...
        ~~~~


# Algebraic types - access and update

* Can use field labels as access functions

    ~~~~ {.haskell}
    getX point = xCoord point
    ~~~~

    * `xCoord` works anywhere you can use a function of type `Point ->
      Double`
    * One consequence: field labels share the same namespace as
      top-level bindings, and must be unique

* There is a special syntax for updating one or more fields

    ~~~~ {.haskell}
    setX point x = point { xCoord = x }
    setXY point x y = point { xCoord = x, yCoord = y }
    ~~~~

    * Obviously doesn't update destructively, but returns new,
      modified `Point`

    * Very handy to maintain state in tail recursive functions and
      `Monads`

# A few Miscellaneous points

* A `!` before a data field type makes it *strict* - i.e., can't be
  thunk

    ~~~~ {.haskell}
    data State = State !Int Int

    data AlgState = AlgState { accumulator :: !Int
                             , otherValue :: Int }
    ~~~~

    * In both cases above, the first `Int` cannot hold a thunk, but
      only a value

    * When initializing an algebraic datatype, it is mandatory to
      initialize all strict fields (since they cannot hold the
      `undefined` thunk).

* [`Data.Map`](http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-Map.html) 
maintains efficient, functional lookup tables
    * The tables cannot be mutated, but can be updated and used in
      recursive functions

* [`words`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:words)
  breaks a `String` up into a list of whitespace-separated words


[RWH]: http://book.realworldhaskell.org/
[Platform]: http://hackage.haskell.org/platform/
[GHCdoc]: http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html
[GHCI]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html
[Hoogle]: http://www.haskell.org/hoogle/
[DMR]: http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5
[DMRWiki]: http://www.haskell.org/haskellwiki/Monomorphism_restriction
[Awkward]: http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf
[default]: http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4
