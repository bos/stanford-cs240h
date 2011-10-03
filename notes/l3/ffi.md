
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
    * But logically there is one `addn` function per invocation of `add`
    * Each `addn` instance is a different `Val`, but all share same
      `ValInfo`
    * Use `args[0]` in each `Val` to specify value of `n`

# Thunk values

* A `Val` with `tag == THUNK` uses the `thunk` field

    ~~~~ {.c}
        Exception *(*thunk) (Val *closure);
    ~~~~

    * *Updates* `v` (turns it into non-thunk) or returns a non-`NULL`
      `Exception *`

* To evaluate a thunk:

    ~~~~ {.c}
            v->thunk->func (v);
    ~~~~

* Two big differences between thunks and functions
    * A function takes an argument, while a thunk does not
    * A function value is immutable, while a thunk updates itself

* Note also that a thunk may throw an exception
    * Functions can, too, but for simplicity let's implement it by
      having the function return a thunk that throws an exception

# Forcing

* Turning a thunk into a non-thunk is known as *forcing* it
* What if a thunk's return value is bigger than thunk?
    * This is why we have the `IND` `ValInfo` tag--Allocate new `Val`,
      place indirect forwarding pointer in old `Val`
* A possible implementation of forcing that walks `IND` pointers

    ~~~~ {.c}
    Exception *force (Val **vp)
    {
      for (;;) {
        if ((*vp)->info->tag == IND)
          (*vp) = (*vp)->arg[0].boxed;
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
* Idea: `closure->args` is list head of previously curried arguments

    ~~~~ {.haskell}
    const3 :: a -> b -> c -> a
    const3 a b c = a
    ~~~~

    * Compiler emits 3 `ValInfo`s and 3 functions for `const3`
    * Top-level binding's `ValInfo` has `func = const3_1`
    * `const3_1` creates `Val` with first argument in `arg[0]` and
      uses the second `ValInfo`, which has `func = const3_2`
    * `const3_2` creates a `Val` where `arg[0]` contains the second
      argument, `arg[1]` points to first `Val`, and `func` is
      `const3_3`
    * `const3_3` has access to all arguments and actually implements
      `const`

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

    * Lets `Int` contain thunk, but avoids pointer chase when strict

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

# Strictness revisited

* Recall `seq :: a -> b -> b`
    * If `seq a b` is forced, then first `a` is forced, then `b`

* Recall strictness flag on fields in data declarations

    ~~~~ {.haskell}
    data IntWrapper = IntWrapper !Int
    ~~~~

    * `Int` has `!` before it, meaning it must be strict
    * Strict means the `Int`'s `ValInfo` cannot have `tag` `THUNK` or `IND`
* Accessing a strict `Int` touches only one cache-line
    * Recall `data Int = I# Int#` has only one constructor
    * Plus strict flag means `tag == CONSTRNO`, so know what's in
      `ValInfo`
    * Plus `Int#` is unboxed
    * Thus, once `IntWrapper` forced, immediately safe to access `Int`
      as

        ~~~~ {.c}
            myIntWrapper.arg[0]->arg[0].unboxed
        ~~~~

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


# `ByteString`s

# `Ptr`

# `hsc2hs`

[GHC.Prim]: http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html
[MagicHash]: http://www.haskell.org/ghc/docs/7.0-latest/html/users_guide/syntax-extns.html#magic-hash
