% CS240H Lab 2

# Hilbert R-tree

Build a purely functional implementation of the
[Hilbert R-tree](http://en.wikipedia.org/wiki/Hilbert_R-tree), a data
structure for managing and querying two-dimensional geometric data,
such as rectangles, circles, and other shapes.

This data structure was introduced in the influential VLDB paper
"[Hilbert R-tree: An Improved R-tree using Fractals](http://scholar.google.com/scholar?q=Hilbert+R-tree+an+improved+r-tree+using+fractals)". You
can find a PDF copy of the original paper at
[cis.temple.edu](http://www.cis.temple.edu/~vasilis/Courses/CIS750/Papers/HilbertRtree-Kamel.pdf).

Your implementation should follow the original description from the
VLDB paper.

## Requirements

You should implement two algorithms:

* Insert

* Search

Your implementation only needs to be able to insert and search
rectangles.  We are not interested in more complex shapes.

Assume that all X and Y coordinates will be specified as integers
between 0 and 65536.


## Sample data

We have provided a [sample rectangle file](rects.txt) containing all
1,454 rectangular features from the
[Visual 6502 project](http://visual6502.org/).


## Command line application

Your submission must come in the form of a command line application.
The application must behave as follows:

* Read a file (whose name will be supplied on the command line) in the
  format used by the sample `rects.txt` file that we have supplied.

* Construct a Hilbert R-tree in memory from this data.

* Measure and print the amount of time needed to read the input file
  and construct the tree.

Once it has started, your application must read "query rectangles"
from `stdin` until end-of-file.

For each query rectangle, it should print out (on `stdout`) three
pieces of information:
  
* The count of the number of rectangles in the tree that overlap with
  the query rectangle.
  
* A small number (say up to 4) of overlapping rectangles.  (Why an
  upper limit? In case the query rectangle overlaps with *all*
  rectangles in the tree!  We don't want to be spammed with huge
  amounts of output in that case.)

* The amount of time needed to perform the query.


~~~~
$ ./my-submission visual486.txt
visual486.txt: 15373 rectangles read in 25.3 milliseconds
>>> 3458,2482,3458,2456,3570,2456,3570,2482
found 2 matches in 14 microseconds:
    3456,2482,3456,2456,3560,2456,3560,2482
    3340,2490,3340,2430,3600,2430,3600,2482


## Submission

You should submit your work in the form of a URL to a publicly
accessible `git` repository, which should include the source and a
`README` file that tells us:

1. Who you are.

1. How to build and run your program.

Unfortunately, due to budget cuts, we cannot afford JPEGs of ponies
any longer, and we will not be awarding quatloo bonuses for extra
achievements.
