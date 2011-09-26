
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
        let q = x / y          -- safe as q never evaluated if y == 0
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

# Some basic list functions

The Prelude defines some list functions approximately as follows:

~~~ {.haskell}
head :: [a] -> a
head (x:_) = x
head []    = error "head: empty list"
~~~

~~~ {.haskell}
tail :: [a] -> a
tail (_:xs) = xs
tail []     = error "tail: empty list"
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
    * Infix application is the default
    * Putting functions in parentheses makes them prefix, e.g., `(+) 1
      2`
* For tuples, prefix constructors are `(,)`, `(,,)`, `(,,,)`, `(,,,,)`, etc.
* Infix functions can be partially applied in a parenthesized
  *section*

    ~~~ {.haskell}
    stripPunctuation :: String -> String
    stripPunctuation = filter (`notElem` "!#$%&*+./<=>?@\\^|-~:")
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
* To install packages for the first lab, you will run

    ~~~
    cabal install http-enumerator utf8-string tagsoup
    ~~~

    * Will install packages in `$HOME/.cabal`, and register them with
      GHC in `$HOME/.ghc`
    * If you ever want to start fresh, must delete both `$HOME/.cabal`
      and `$HOME/.ghc`


# Modules and `import` syntax

* Haskell groups top-level bindings into *modules*
    * The default module name is `Main`, because programs start at
      function `main` in `Main`
    * Except for `Main`, a module named *M* must reside in a file
      named *M*`.hs`
    * Module names are capitalized; I generally lower-case file names
      of `Main` modules
* Let's add this to the top of our source file

    ~~~~ {.haskell}
    module Main where      -- redundant since Main is the default
    import qualified Data.ByteString.Lazy.UTF8 as L
    import Data.Char
    import Network.HTTP.Enumerator (simpleHttp)
    import System.Environment
    ~~~~

    * `import` *module* - imports all symbols in *module*
    * `import qualified` *module* `as` *ID* - prefixes imported symbols
      with *ID*`.`
    * `import` *module* `(`*function1*[`,` *function2* ...]`)` -
      imports just the named functions

# `do` notation

* Let's write a program to dump a web page

~~~ {.haskell}
main = do
  (url:_) <- getArgs        -- Sets url to first command-line argument
  page <- simpleHttp url    -- Sets page to contents as a ByteString
  putStr (L.toString page)  -- Converts ByteString to String and prints it
~~~

* This task requires some impure (non-functional) actions
    * Extracting command-line args, Creating a TCP connection, Writing
      to stdout
* A `do` block lets you sequence IO actions.  In a `do` block:
    * <span style="color:blue">*pat* `<-` *action*</span> -- binds
      *pat* (variable or constructor pattern) to result of executing
      **action*
    * <span style="color:blue">`let` *pat* `=` *pure-value*</span> --
    binds *pat* to *pure-value* (no "`in` ..." required)
    * <span style="color:blue">*action*</span> -- executes *action* and
      discards the result, or returns it if at end of block

# What are the types of IO actions?

~~~~ {.haskell}
main :: IO ()
getArgs :: IO [String]
simpleHttp :: String -> IO L.ByteString   -- in reality more polymorphic 
putStr :: String -> IO ()
~~~~

* `IO` is a parameterized type (just as `Maybe` is parameterized)
    * "`IO [String]`" means IO action that, if executed, produces a
      value of type `[String]`
    * Unlike `Maybe`, we won't see any constructors for `IO`, which is
      somewhat magic
* What if we try to print the first command-line argument as follows?

    ~~~~ {.haskell}
    main = putStr (head getArgs)
    ~~~~

    * Oops, head expects type `[String]`, not `IO [String]`

* How to de-construct an `IO [String]` to get a `[String]`
    * We can't use `case`, because we don't have a constructor for
      `IO`<br> ... Besides, order of IO actions is important, while
      bindings are order-independent
    * That's the point of the `<-` operator in `do` blocks!


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

* Let's combine `simpleHttp` and `L.toString` into one function

    ~~~~ {.haskell}
    simpleHttpStr :: String -> IO String
    simpleHttpStr url = do
      page <- simpleHttp url
      return (L.toString page)
    ~~~~

    * The return value of a `do` block is that of its last action
* Note:  **`return` is not control flow statement**, just a function

    ~~~~ {.haskell}
    return :: a -> IO a
    ~~~~

    * Every action in an `IO` do block must have type `IO a` for some
      `a`
    * `L.toString` returns a `String`, use `return` to make an `IO
      String`
    * In a do block, "`let x = e`" is like "`x <- return e`" (unless
      `e` contains symbol `x`)


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
* `do` blocks are just syntactic sugar for calling `>>=`
    * Let's de-sugar our original `main`:

    ~~~~ {.haskell}
    main =
        getArgs >>= \(url:_) ->
        simpleHttp url >>= \page ->
        putStr (L.toString page)
    ~~~~



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

# Ad hoc polymorphism





[RWH]: http://book.realworldhaskell.org/
[Platform]: http://hackage.haskell.org/platform/
[GHCdoc]: http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html
[GHCI]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html
[Hoogle]: http://www.haskell.org/hoogle/


<!--

Declarations

Some data type basics

Things to mention:

  - emacs mode
  - hoogle
  - :i for fixity
  - Ad hoc polymorphism

-->
