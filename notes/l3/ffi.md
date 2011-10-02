
# Haskell data representation

* Let's make a na&#xef;ve implementation of Haskell values in C

* A value requires a constructor, plus arguments

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

* `Val`s with `ValInfo` tag `FUNC` use `func` field

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

* `Val`s with `ValInfo` tag `THUNK` use `thunk` field

    ~~~~ {.c}
        Exception *(*thunk) (Val *closure);
    ~~~~

    Calling `v->info->thunk (v)` does one of two things:
    * *Updates* `v`--i.e., turns it into a non-thunk `Val`--and
       returns `NULL`, or
    * Returns a non-`NULL` `Exception *`

# Boxed and unboxed types

* `Val` is kind of like a *box* containing values

    ~~~~ {.c}
    union Arg {
      struct Val *boxed;     /* most values like this */
      unsigned long unboxed; /* "primitive" values */
    };

    typedef struct Val {
      const struct ValInfo *info;
      union Arg *args[];
    } Val;
    ~~~~

# `ByteString`s

# `Ptr`

# `hsc2hs`
