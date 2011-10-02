
# Na&#xef;ve Haskell data representation

* A value requires a constructor, plus arguments

    * At runtime, need to determine a value's constructor, but not
      it's type<br/> (Compiler type-checks program, no runtime type
      checks)

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

# Function and thunk values

* A `Val` whose `ValInfo` has `tag == FUNC` uses the `func` field

    ~~~~ {.c}
        Val *(*func) (const Val *closure, const Val *arg);
    ~~~~

    * Since we have currying, assume all functions take one
      argument<br/>
      (for performance, real compilers optimize multi-argument case)
    * To apply function `f` to argument `a`, where both are type `Val
      *`:

        ~~~~ {.c}
                f->info->func (f, a);
        ~~~~

    * Use `f->args` for list head of previously curried arguments

* A `Val`s with `tag == THUNK` uses the `thunk` field

    ~~~~ {.c}
        Exception *(*thunk) (Val *closure);
    ~~~~

    Calling `v->info->thunk (v)` does one of two things:
    * *Updates* `v`--i.e., turns it into a non-thunk `Val`--and
       returns `NULL`, or
    * Returns a non-`NULL` `Exception *`

# Unboxed types

* Unfortunately, now `Int` has even more overhead
    * To use, must check `i->info->tag` then access `i->info->constr`
    * Each number needs a distinct `ValInfo` structure

* Idea: Have special *unboxed* types that don't use `struct Val`

    ~~~~ {.c}
    union Arg {
      struct Val *boxed;     /* most values like this */
      unsigned long unboxed; /* "primitive" values */
    };

    typedef struct Val {
      const struct ValInfo *info;
      union Arg *args[];  /* each arg now boxed or unboxed */
    } Val;
    ~~~~

    * Unboxed types have no constructor and cannot be thunks
    * Can fit in a single register or take the place of a `Val *` arg
    * Extend `GCInfo` to identify which args are and are not boxed


# Unboxed types in GHC

* GHC exposes unboxed types (even though not part of Haskell)
    * Symbols use `#` character--must enable with `-XMagicHash` option
    * Have use unboxed types (`Int#`) and primitive operations
      (`+#`)
    * List unboxed types/ops with GHCI command "`:browse GHC.Prim`"
    * Also have unboxed constants--`2#`, `'a'#`, `2##` (unsigned),
      `2.0##`

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

# Restrictions on unboxed types

* Cannot instantiate type variables with unboxed types

    ~~~~ {.haskell}
    {-# LANGUAGE MagicHash #-}
    import GHC.Prim

    data FastPoint = FastPoint Double# Double#  -- ok
    fp = FastPoint 2.0## 2.0##                  -- ok

    -- Error: cannot pass unboxed type to polymorphic function
    fp' = FastPoint 2.0## (id 2.0##)

    -- Error: cannot use unboxed type as type paremeter
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


# `ByteString`s

# `Ptr`

# `hsc2hs`
