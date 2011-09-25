
# CS240h: Functional systems in Haskell

* I'm David Mazi&egrave;res
    * Spent most of my career working on OSes, Systems, and Security
    * Previously used C++ and C, but started using Haskell a couple of
      years ago
    * Course partly inspired by my experience learning Haskell

* Co-teaching with Bryan O'Sullivan
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

* Haskell lets you realize new types of functionality

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
    #. A large final project & presentation
    #. Class attendance and participation

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

    * Bindings are separated by "`;`" character, usually auto-inserted
      by
      [layout](http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7)
      rule

* A binding may declare a *function* of one or more arguments
    * A function and its arguments are separated by white space
      (both when defining and when invoking it)

    ~~~ {.haskell}
    add arg1 arg2 = arg1 + arg2   -- defines function add
    five = add 2 3                -- invokes function add
    ~~~

* Parentheses can wrap compound expressions, must do so for arguments

    ~~~ {.haskell}
    bad = print add 2 3     -- error! (print should have only 1 argument)
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

    * *order-independent* - bindings can be evaluated as needed, in
       any order
    * *lazy* - definitions of unused symbols are not evaluated

    ~~~ {.haskell}
    safeDiv x y =
        let q = x / y          -- safe as q never evaluated if y == 0
        in if y == 0 then 0 else q
    main = print (safeDiv 1 0) -- prints 0
    ~~~

    * *recursive* - the bound symbol is in scope within its own
       definition

    ~~~ {.haskell}
    x = 5                 -- this x is never used in main

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
    * Unfortunately, `factorial` multiplies by `n` *after* evaluating
      `factorial (n-1)`
* Idea: use *accumulator* argument to make calls tail recursive

    ~~~ {.haskell}
    factorial n = let loop acc n' = if n' > 1
                                    then loop (acc * n') (n' - 1)
                                    else acc
                  in loop 1 n
    ~~~ 

    * Here `loop` is tail recursive, compiles to an actual loop

# Guards and `where` clauses

* Can shorten function declarations with *guards*:

    ~~~ {.haskell}
    factorial n = let loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
                                  | otherwise = acc
                  in loop 1 n
    ~~~

    * "`|`" symbol introduces a guard
    * Guards evaluated top to bottom, first `True` guard wins
    * System library defines `otherwise = True`

* Bindings can also end with `where` clauses--like inverted `let`

    ~~~ {.haskell}
    factorial n = loop 1 n
        where loop acc n' | n' > 1    = loop (acc * n') (n' - 1)
                          | otherwise = acc
    ~~~

    * Unlike `let`, a `where` clause scopes over multiple guarded
      definitions


# Tip: variable names

* Inner functions (e.g., `loop`) often have arguments related to
  outer function
    * Compiler will warn if you re-use symbol name & shadow binding
    * Typical practice is to add `'` ("prime") to inner-function's symbol
    * Haskell accepts the `'` character in symbols, except as first
      character
* Personally, I find this practice a bit error-prone
    * While learning Haskell, I repeatedly made the error of omitting
      prime, e.g.:

    ~~~ {.haskell}
    factorial n = loop 1 n
        where loop acc n' | n' > 1    = loop (acc * n) (n' - 1) -- bug
                          | otherwise = acc
    ~~~

    * Can avoid problem by using the longer symbol name for the outer function

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
    * `Integer` - an arbitrary-size integer
    * *type1* `->` *type1* - a function from *type1* to *type2*
* You can declare the type of a symbol or expression with `::`

    ~~~ {.haskell}
    x :: Integer
    x = 2
    addx y = x + y
    ~~~

* Usually, the compiler can infer types--ask [GHCI][GHCI] to see
  inferred type:

    ~~~
    *Main> :t addx
    addx :: Integer -> Integer
    ~~~

    * But it is good practice to declare types of top-level bindings

# User-defined data types



[RWH]: http://book.realworldhaskell.org/
[Platform]: http://hackage.haskell.org/platform/
[GHCdoc]: http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html
[GHCI]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html


<!--

Declarations

Some data type basics

Things to mention:

  - emacs mode
  - hoogle
  - :i for fixity
  - layout
-->
