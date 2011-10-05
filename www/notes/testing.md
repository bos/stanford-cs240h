% Testing

# The dominant paradigm

By far the most widely used style of testing today is _unit_ testing.

* Invent a "state of the world".

* Run the unit we're testing (e.g. a function).

* Check the modified state of the world to see if it looks like it
  should.
  
* Profit!


# Shameless Wikipedia pillaging

If you're used to unit testing, this may look reasonable.

~~~~ {.java}
public class TestAdder {
    public void testSum() {
        Adder adder = new AdderImpl();
        assert(adder.add(1, 1) == 2);
        assert(adder.add(1, 2) == 3);
        assert(adder.add(2, 2) == 4);
        assert(adder.add(0, 0) == 0);
        assert(adder.add(-1, -2) == -3);
        assert(adder.add(-1, 1) == 0);
        assert(adder.add(1234, 988) == 2222);
    }
}
~~~~

The "real world" adds tons of cruft: mock objects, preprocessor abuse
to expose "test seams", etc.


# Problem: unit testing is only as good as your patience

The example from the previous slide contains 7 tests.

It's not hard to imagine a world in which we lose the will to continue
inventing new unit tests long before we've exhausted our search of the
space of possible bugs.


# Let's sort a list

~~~~ {.haskell}
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort pred = go
  where
    go []  = []
    go [x] = [x]
    go xs  = merge (go xs1) (go xs2)
      where (xs1,xs2) = split xs

    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | pred x y  = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
~~~~ {.haskell}


# The split function

The purpose of splitting is to produce two sublists of roughly equal
length, so that they can be merged (where the sorting occurs).

~~~~ {.haskell}
split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:xs,y:ys)
  where (xs,ys) = split zs
~~~~ {.haskell}


# Sorting: a unit test perspective

~~~~
sort [1,2,3,4] == [1,2,3,4]
sort [4,3,2,1] == [1,2,3]
sort [1,4,2,3] == [1,2,3]
...
~~~~

