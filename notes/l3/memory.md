
# Na&#xef;ve Haskell data representation

* A value requires a constructor, plus arguments

    * At runtime, need to determine a value's constructor, but not
      it's type<br/> (Compiler already type-checked program, so no
      runtime type checks)

    ~~~~ {.c}
    struct Val {
      unsigned long constrno; /* constructor # */
      struct Val *args[];     /* flexible array */
    }
    ~~~~

    * For a type like `[Int]`, `constrno` might be 0 for `[]` and 1
      for `(:)`, where `[]` has 0-sized `args` and `(:)` has 2-element
      `args`
    * For a type like `Int`, `constrno` can be the actual integer,
      with no `args`
    * For a single-constructor type (e.g., `Point`) `constrno` not
      used

* Problems with our approach so far
    * No way to represent exceptions or thunks
    * Garbage collector needs to know how many elements are in `args`
    * Small values such as `Int`s always require chasing a pointer

# Metadata for values

* Let's add a level of indirection to describe values

    ~~~~ {.c}
    typedef struct Val {
      const struct ValInfo *info;
      struct Val *args[];
    } Val;

    struct ValInfo {
      struct GCInfo gcInfo;  /* for garbage collector */
      enum { THUNK, CONSTRNO, FUNC, IND } tag;
      union {
        Exception *(*thunk) (Val *closure);
        unsigned int constrno;
        Val *(*func) (const Val *closure, const Val *arg);
      };
    };
    ~~~~

    * `gcInfo` says how many `Val *`s are in `args` and where they are
    * `tag == CONSTRNO` means `constrno` valid, used as on last slide
    * `tag == IND` means `args[0]` is an indirect *forwarding pointer*
      to another `Val` and union is unused; useful if size of `args`
      grows

# Function values

* A `Val` whose `ValInfo` has `tag == FUNC` uses the `func` field

    ~~~~ {.c}
        Val *(*func) (const Val *closure, const Val *arg);
    ~~~~

* `closure` is the `Val` whose `ValInfo` contains `func`
    * Provides an environment so `ValInfo`/`func` can be re-used
* `arg` is the function argument
* Assume all functions take one argument
    * Logically this is fine since we have currying
    * For performance, real compilers must optimize multi-argument case
* To apply function `f` to argument `a`, where both are type `Val *`:

    ~~~~ {.c}
            f->info->func (f, a);
    ~~~~


# Closures

* Top-level bindings don't need closures

    ~~~~ {.haskell}
    addOne :: Int -> Int
    addOne x = x + 1
    ~~~~

    * The `Val` for function `addOne` can have zero-length `args`

* Local bindings may need environment values in `closure`

    ~~~~ {.haskell}
    add :: Int -> (Int -> Int)
    add n = \m -> addn m
        where addn m = n + m
    ~~~~

    * Compiler will only emit code for local function `addn` once
    * But logically, there is a separate `addn` function (with a
      different `n`) for each invocation of `add`
    * Each `addn` instance is a different `Val`, but all share same
      `ValInfo`
    * Use `args[0]` in each `Val` to specify value of `n`

# Thunk values

* A `Val` with `tag == THUNK` uses the `thunk` field in `ValInfo`

    ~~~~ {.c}
        Exception *(*thunk) (Val *closure);
    ~~~~

    * *Updates* `v` (turns it into non-thunk) or returns a non-`NULL`
      `Exception *`

* To evaluate a thunk:

    ~~~~ {.c}
            v->info->thunk (v);
    ~~~~

* Two big differences between thunks and functions
    * A function takes an argument, while a thunk does not
    * A function value is immutable, while a thunk updates itself

* Note also that a thunk may throw an exception
    * Functions can, too, but for simplicity let's implement it by
      having the function return a thunk that throws an exception

# Forcing

* Turning a thunk into a non-thunk is known as *forcing* it
* What if a thunk's return value doesn't fit in thunk's `args`?
    * This is why we have the `IND` `ValInfo` tag--Allocate new `Val`,
      place indirect forwarding pointer in old `Val`
* A possible implementation of forcing that walks `IND` pointers:

    ~~~~ {.c}
    Exception *force (Val **vp)
    {
      for (;;) {
        if ((*vp)->info->tag == IND)
          *vp = (*vp)->arg[0].boxed;
        else if ((*vp)->info->tag == THUNK) {
          Exception *e = (*vp)->info->thunk (*vp);
          if (e)
            return e;
        }
        else
          return NULL;
      }
    }
    ~~~~


# Currying

* Let's use simple implementation of currying (GHC very complex)
* Set `closure->args` to head of list of previously curried args

    ~~~~ {.haskell}
    const3 :: a -> b -> c -> a
    const3 a b c = a
    ~~~~

    * Compiler emits 3 `ValInfo`s and 3 functions for `const3`
    * Top-level binding's `ValInfo` has `func = const3_1`
    * `const3_1` creates `Val` where `arg[0]` is first argument (`a`)
      and <span style="white-space: nowrap;">`info->func =
      const3_2`</span>
    * `const3_2` creates a `Val` where `arg[0]` is the second argument
      (`b`), `arg[1]` is `closure`, and `info->func` is `const3_3`
    * `const3_3` has access to all arguments and actually implements
      `const3`

* Shared arguments have common arg tails, only evaluated once

    ~~~~ {.haskell}
        let f = const3 (superExpensive 5) -- evaluated once
        in (f 1 2, f 3 4)
    ~~~~

# Unboxed types

* Unfortunately, now `Int` has even more overhead
    * To use, must check `i->info->tag` then access `i->info->constr`
    * Moreover, each number needs a distinct `ValInfo` structure

* Idea: Have special *unboxed* types that don't use `struct Val`

    ~~~~ {.c}
    union Arg {
      struct Val *boxed;     /* most values are boxed */
      unsigned long unboxed; /* "primitive" values */
    };

    typedef struct Val {
      const struct ValInfo *info;
      union Arg *args[];  /* args can be boxed or unboxed */
    } Val;
    ~~~~

    * Unboxed types have no constructor and cannot be thunks
    * Can fit in a single register or take the place of a `Val *` arg
    * Must extend `GCInfo` to identify which args are and are not boxed


# Unboxed types in GHC

* GHC exposes unboxed types (even though not part of Haskell)
    * Symbols use `#` character--must enable with
      [`-XMagicHash`][MagicHash] option
    * Have unboxed types (`Int#`) and primitive operations on them
      (`+#`)
    * See [GHC.Prim][GHC.Prim] or type "`:browse GHC.Prim`" in GHCI
    * Also have [unboxed constants][MagicHash]--`2#`, `'a'#`, `2##`
      (unsigned), `2.0##`

* What is `Int` really?
    * Single-constructor data type, with a single, unboxed argument

    ~~~~
    Prelude> :set -XMagicHash
    Prelude> :m +GHC.Types GHC.Prim
    Prelude GHC.Types GHC.Prim> :i Int
    data Int = I# Int#      -- Defined in GHC.Types
    ...
    Prelude GHC.Types GHC.Prim> case 1 of I# u -> I# (u +# 2#)
    3
    ~~~~

    * Lets `Int` contain thunk, but avoids pointer dereference once
      evaluated

# Restrictions on unboxed types

* Cannot instantiate type variables with unboxed types

    ~~~~ {.haskell}
    {-# LANGUAGE MagicHash #-}
    import GHC.Prim

    data FastPoint = FastPoint Double# Double#  -- ok
    fp = FastPoint 2.0## 2.0##                  -- ok

    -- Error: can't pass unboxed type to polymorphic function
    fp' = FastPoint 2.0## (id 2.0##)

    -- Error: can't use unboxed type as type paremeter
    noInt :: Maybe Int#
    noInt = Nothing
    ~~~~

* Enforced by making unboxed types a different kind of type

    ~~~~
    Prelude GHC.Types GHC.Prim> :kind Int#
    Int# :: #
    ~~~~

    * Recall type variables have kinds with stars (&#x2217;, &#x2217;
      &#x2192; &#x2217;, etc.), never `#`

# `seq` revisited

* Recall `seq :: a -> b -> b`
    * If `seq a b` is forced, then first `a` is forced, then `b` is
      forced and returned
* Consider the following code:

    ~~~~ {.haskell}
    infiniteLoop = infiniteLoop :: Char   -- loops forever

    seqTest1 = infiniteLoop `seq` "Hello" -- loops forever

    seqTest2 = str `seq` length str       -- returns 6
        where str = infiniteLoop:"Hello"
    ~~~~

    * `seqTest1` hangs forever, while `seqTest2` happily returns 6
* `seq` only forces a `Val`, not the `arg` fields of the `Val`
    * `seqTest2`'s `seq` forces `str`'s constructor `(:)`, but not the
      head or tail
    * This is known as putting `str` in *Weak Head Normal Form* (WHNF)
    * Can't fully evaluate an arbitrary data type (but see
      [Control.DeepSeq](http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html))


# Example: `seq` implementation

~~~~ {.haskell}
Val *seq_2 (Val *a, Val *b)
{ /* assume seq_1 put first arg in a */
  val = xmalloc (offsetof (Val, args[2]));
  val->info = &seq_info;
  val->args[0] = a->args[0];
  val->args[1] = b->args[0];
  return val;
}

struct ValInfo seq_info = {
  some_gcinfo, THUNK, .thunk = &seq_thunk
};

Exception *seq_thunk (Void *c)
{
  Exception *e = force (&c->args[0]);
  if (!e) {
    c->info = &ind_info;     /* ValInfo with tag IND */
    c->args[0] = c->args[1]; /* forward to b */
  }
  return e;
}
~~~~

# Strictness revisited

* Recall strictness flag on fields in data declarations

    ~~~~ {.haskell}
    data IntWrapper = IntWrapper !Int
    ~~~~

    * `Int` has `!` before it, meaning it must be strict
    * Strict means the `Int`'s `ValInfo` cannot have `tag` `THUNK` or `IND`
* Accessing a strict `Int` touches only one cache line
    * Recall `data Int = I# Int#` has only one constructor
    * Plus strict flag means `tag == CONSTRNO`, so know what's in
      `ValInfo`
    * Plus `Int#` is unboxed
    * Thus, once `IntWrapper` forced, immediately safe to access `Int`
      as

        ~~~~ {.c}
            myIntWrapper.arg[0]->arg[0].unboxed
        ~~~~

# Semantic effects of strictness

* Strictness is primarily used for optimization
    * To avoid building up long chains of thunks
    * To save overhead of checking whether thunk evaluated
* But has semantic effects:  A non-strict `Int` is not just a number
    * Can also throw an exception or loop forever when evaluated
    * Such behavior can be modeled as a special value $\bot$
      ("bottom")
    * So the values of `Int` are $\{0,1\}^{64} \cup \{\bot\}$
    * Types that include value $\bot$ are called *lifted*
* Note 1: an unboxed type is necessarily unlifted
* Note 2: `!Int` not a first-class type, only valid for `data` fields

    ~~~~ {.haskell}
    data SMaybe a = SJust !a | SNothing   -- ok, data field
    strictAdd :: !Int -> !Int -> !Int     -- error
    type StrictMaybeInt = Maybe !Int      -- error
    ~~~~

# `case` statements revisited

* `case` statement pattern matching can force thunks
    * An *irrefutable* pattern is one that always matches
    * A pattern consisting of a single variable or `_` is irrefutable
    * Any non-irrefutable pattern forces evaluation of the argument
    * Matching happens top-to-bottom, and left-to-right within
      alternatives
* Function pattern matching is the same as (desuggared into) `case`
    * `undefined :: a` is `Prelude` symbol with value $\bot$, handy
      for testing

    ~~~~ {.haskell}
    f ('a':'b':rest) = rest
    f _              = "ok"
    test1 = f (undefined:[])   -- error
    test2 = f ('a':undefined)  -- error
    test3 = f ('x':undefined)  -- "ok" (didn't force tail)
    ~~~~

* Adding `~` before a pattern makes it irrefutable

    ~~~~ {.haskell}
    three = (\ ~(h:t) -> 3) undefined  -- evaluates to 3
    ~~~~

# `newtype` declarations

* We've seen two ways to introduce new types
    * `data` -- creates a new (boxed) type, adding overhead of a `Val`
      wrapper
    * `type` -- creates an alias for an existing type, with no overhead
* Sometimes you want a new type implemented by an existing type
    * E.g., might want `Meters`, `Seconds`, `Grams`, all implemented
      by `Double`
    * Using `type` would make them all synonymous, facilitating errors
    * Might want different instances of `Show` for each, impossible
      with `type`
    * Could say `data Meters = Meters Double` -- but will add overhead
* The `newtype` keyword introduces new type with no overhead
    * Use just like `data`, but limited to one constructor and one
      field
    * This is possible because all type-checking is compile-time

# `newtype` semantics

* What's the semantic difference between these two declarations?

    ~~~~ {.haskell}
    newtype NTInt = NTInt Int deriving (Show)
    ~~~~

    ~~~~ {.haskell}
    data SInt = SInt !Int deriving (Show)
    ~~~~

* The `NTInt` constructor is a "fake" compile-time-only construct
    * A case statement deconstructing a `newtype` compiles to nothing

    ~~~~ {.haskell}
    newtype NTInt = NTInt Int deriving (Show)
    uNTInt = NTInt undefined
    testNT = case uNTInt of NTInt _ -> True   -- returns True

    data SInt = SInt !Int deriving (Show)
    uSInt = SInt undefined
    testS = case uSInt of SInt _ -> True      -- undefined
    ~~~~

# The [UNPACK][UNPACK] pragma

* `newtype` almost always better than `data` when it applies
* What about a multi-field data type?

    ~~~~ {.haskell}
    data TwoInts = TwoInts !Int !Int
    ~~~~

    * Fields are strict, we know they'll have `CONSTRNO` `ValInfo`
    * Why not stick the `Int#`s directly into the `args` of a
      `TwoInts` `Val`?
    * GHC provides an `UNPACK` pragma to do just this

        ~~~~ {.haskell}
        data TwoInts = TwoInts {-# UNPACK #-} !Int {-# UNPACK #-} !Int
        ~~~~

    * Works for any strict field with a single-constructor datatype
* Unlike `newtype`, `UNPACK` is not always a win
    * If you pass field as argument, will need to re-box it
* `-funbox-strict-fields` flag unpacks *all* strict fields


# User-managed memory

* Opaque type [`Ptr a`][Ptr] represents pointers to type `a`
    * Pointers are not typesafe--allow pointer arithmetic and casting

        ~~~~ {.haskell}
        nullPtr :: Ptr a
        plusPtr :: Ptr a -> Int -> Ptr b
        minusPtr :: Ptr a -> Ptr b -> Int
        castPtr :: Ptr a -> Ptr b
        ~~~~

    * Pointer arithmetic is always in units of bytes (unlike in C,
      where unit is size of the pointed-to object)
* Class [`Storable`][Storable] provides raw access to memory using
  `Ptr`s

    ~~~~ {.haskell}
    class Storable a where
        sizeOf :: a -> Int
        alignment :: a -> Int
        peek :: Ptr a -> IO a
        poke :: Ptr a -> a -> IO ()
        ...
    ~~~~

    * Most basic types (`Bool`, `Int`, `Char`, `Ptr a`, etc.) are `Storable`

# `alloca`

* Easiest way to get a valid `Ptr` is `alloca`:

    ~~~~ {.haskell}
    alloca :: Storable a => (Ptr a -> IO b) -> IO b
    ~~~~

    * Allocates enough space for an object of type `a`
    * Calls function with a `Ptr` to the space
    * Reclaims the memory when the function returns (much like C
      `alloca`)
    * Can also ask for a specific number of bytes:

    ~~~~ {.haskell}
    allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
    ~~~~

* `Foreign` module provides handy [`with`][with] utility

    ~~~~ {.haskell}
    with :: Storable a => a -> (Ptr a -> IO b) -> IO b
    with val f  =
      alloca $ \ptr -> do
        poke ptr val
        res <- f ptr
        return res
    ~~~~


# More `Storable` types

* `Foreign.C` contains wrappers for C type
    * `CInt`, `CUInt`, `CChar`, `CDouble`, `CIntPtr` etc.
* `Data.Int` and `Data.Word` have all sizes of machine integer
    * `Int8`, `Int16`, `Int32`, `Int64` -- signed integers
    * `Word8`, `Word16`, `Word32`, `Word64` -- unsigned integers

* Example: extract all the bytes from a `Storable` object

    ~~~~ {.haskell}
    toBytes :: (Storable a) => a -> [Word8]
    toBytes a = unsafePerformIO $
        with a $ \pa -> go (castPtr pa) (pa `plusPtr` sizeOf a)
        where go p e | p < e = do b <- peek p
                                  bs <- go (p `plusPtr` 1) e
                                  return (b:bs)
                     | otherwise = return []
    ~~~~

    * `unsafePerformIO` might be okay here since `tobytes` pure
    * Notice how `plusPtr` lets us change from `Ptr a` to `Ptr Word8`

# `malloc` and `mallocForeignPtr`

* Can also allocate longer-lived memory with `malloc`

    ~~~~ {.haskell}
    malloc :: Storable a => IO (Ptr a)
    mallocBytes :: Int -> IO (Ptr a)
    free :: Ptr a -> IO ()
    realloc :: Storable b => Ptr a -> IO (Ptr b)
    reallocBytes :: Ptr a -> Int -> IO (Ptr a)
    ~~~~

    * Disadvantage:  bad programming can lead to memory
      leaks/corruption

* `ForeignPtr` lets you delegate deallocation to garbage collector

    ~~~~ {.haskell}
    mallocForeignPtr :: Storable a => IO (ForeignPtr a)
    mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
    ~~~~

# Working with `ForeignPtr`s

* To use `ForeignPtr`, must convert it to `Ptr`
    * Problem: How does GC know `ForeignPtr` in scope when using
      `Ptr`?
    * Solution: use `Ptr` within function that keeps reference to
      `ForeignPtr`

    ~~~~ {.haskell}
    withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
    ~~~~

* Can also convert `Ptr`s to `ForeignPtr`s

    ~~~~ {.haskell}
    type FinalizerPtr a = FunPtr (Ptr a -> IO ())
    newForeignPtr :: FinalizerPtr a -> Ptr a
                  -> IO (ForeignPtr a)
    newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
    addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a
                           -> IO ()
    ~~~~

    * Can add multiple finalizers, will run in reverse order
* Note use of `FunPtr` -- this is type wrapper for C function pointer
    * Need foreign function interface to create these


# [Foreign function interface][FFI] (FFI)

* Can import foreign functions like this:

    ~~~~ {.haskell}
    foreign import ccall unsafe "stdlib.h malloc"
        c_malloc :: CSize -> IO (Ptr a)
    foreign import ccall unsafe "stdlib.h free"
        c_free :: Ptr a -> IO ()
    ~~~~

    * `ccall` says use C calling convention (also `cplusplus` and few
      others)
    * `unsafe` promises the C function will not call back into
      Haskell
    * `unafe` faster than `safe`, but gives undefined results if call
      triggers GC
* Spec for import string: `"`[`static`] [*c-header*] [`&`][*c-name*]`"`
    * `static` required only if *c-name* is `dynamic` or `wrapper`
    * *c-header* is a single `.h` file with the declaration
       (ignored by GHC)
    * '&' imports pointer rather than function (required for `FunPtr`s)


# FFI types

* FFI function arguments must be *basic foreign types*
    * `Char`, `Int`, `Double`, `Float`, `Bool`, `Int8`, `Int16`,
      `Int32`, `Int64`, `Word8`, `Word16`, `Word32`, `Word64`, `Ptr`
      `a`, `FunPtr a`, and `StablePtr a`
    * Also accepts any `type` or `newtype` wrappers for basic types
      (`CInt`, `CChar`, etc.)<br/>
      [Documentation incorrectly says `data CInt`, but `:i` in GHCI
      reveals truth.]
* FFI function results can be
    * Any valid argument type
    * `()` (for functions returning `void`)
    * `IO a` where `a` is either of the above two
* Place result `IO` if function has side effects or non-determinism
    * Okay to omit if it is a pure C function:

        ~~~~ {.haskell}
        foreign import ccall unsafe "arpa/inet.h ntohl"
            ntohl :: Word32 -> Word32
        ~~~~

    * Haskell can't check C purity, so omitting `IO` can cause
      problems

# [`hsc2hs`][hsc2hs]

* How to access C data structures?

    ~~~~ {.c}
    struct mystruct {
      char *name;
      int value;
    };
    ~~~~

    * Might model with opaque placeholder type

    ~~~~ {.haskell}
    data MyStruct        -- no constructors, just a placeholder

    getValue :: Ptr MyStruct -> IO CInt
    getValue ptr = peek $ ptr `plusPtr` 8  -- assumes char * 8 bytes
    ~~~~

* [`hsc2hs`][hsc2hs] is pre-processor that lets you compute C values

    ~~~~ {.haskell}
    getValue ptr = peek $ ptr `plusPtr`
                   #{offset struct mystruct, value}
    ~~~~

    * Super-simple implementation just uses C macros & `printf`
    * Find the file [`template-hsc.h`][template-hsc.h] on your system
      to see defs of `#` commands
    * Can also define your own macros

# `ByteString`s

[Ptr]: http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Foreign-Ptr.html#t:Ptr
[Storable]: http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Foreign-Storable.html#t:Storable
[GHC.Prim]: http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html
[MagicHash]: http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#magic-hash
[UNPACK]: http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#unpack-pragma
[with]: http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Foreign-Marshal-Utils.html#v:with
[FFI]: http://www.haskell.org/onlinereport/haskell2010/haskellch8.html
[hsc2hs]: http://www.haskell.org/ghc/docs/latest/html/users_guide/hsc2hs.html
[template-hsc.h]: http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=hsc2hs.git;a=blob;f=template-hsc.h;hb=HEAD
