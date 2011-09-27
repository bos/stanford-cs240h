% [CS240h](..) Haskell resources

The class [lecture notes](../notes/) are full of hyperlinks.  If you
want more information on a topic covered in lecture, start by clicking
through any links on the slides.

## General Haskell reference documents

* [*Real World Haskell*](http://book.realworldhaskell.org/)

* [A Gentle Introduction to Haskell](http://www.haskell.org/tutorial/)
\- Not particularly gentle despite the title, but covers the basics
pretty thoroughly

* [Haskell 2010 Language Report](http://www.haskell.org/onlinereport/haskell2010/)
\- The normative language specification.  We don't necessarily
recommend reading it cover-to-cover, but when you want a definitive
answer to something about the language, this is the pace to turn.

## Finding Haskell code

* [Hackage](http://hackage.haskell.org/packages/archive/pkg-list.html)
  has a large collection of Haskell packages.

* [Cabal](http://www.haskell.org/ghc/docs/7.0-latest/html/Cabal/index.html) 
\- The main tool for searching the Hackage database and installing
packages.  The `cabal` program ships with the
[Haskell Platform][Platform].  A few tips:
    * Cabal installs packages in `$HOME/.cabal` and tells GHC how to
      find them by modifying `$HOME/.ghc`.  You must delete both
      directories if you want to start over with a "clean slate".  Bad
      things will happen if you delete `$HOME/.cabal` but not
      `$HOME/.ghc`.
    * Before using cabal, you must run `cabal update` to initialize
      your package database and create a `$HOME/.cabal` directory.
    * After running `cabal update` but before installing any packages,
      it is highly recommended that you edit `$HOME/.cabal/config` to
      set the following configuration options:

            documentation: True
            library-profiling: True

        `documentation` will install documentation in your home
        directory.  You may want to visit
        `$HOME/.cabal/share/doc/index.html` and bookmark that page in
        your browser.

        `library-profiling` will allow you to profile your
        executables, but more importantly allows you to get something
        resembling a stack trace if you are trying to debug an
        unanticipated exception.
      

* [Hoogle](http://www.haskell.org/hoogle/) - the absolute
  must-bookmark site.  Click **Search plugin** to create a bookmark,
  or manually add a link with a short search keyword (such as `ho`)
  to:

            http://www.haskell.org/hoogle/?hoogle=%s

* [Hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html) - similar to
  Hoogle.  Searches are slower, but it indexes more packages.  The URL
  for a keyword bookmark is

            http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=%s

## GHC information

* The main
  [GHC documentation site](http://haskell.org/haskellwiki/GHC).

* The
[GHC User's guide](http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html),
including a chapter on
[GHCI](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html).

* The
[standard libraries](http://www.haskell.org/ghc/docs/latest/html/libraries/index.html)
that ship with GHC, including the
[base](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/index.html)
and
[ghc-prim](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-prim-0.2.0.0/index.html)
packages.  The documentation links to source code.  If you fiddle with
URLs, you also can find HTMLified
[source code](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/src/)
for modules such as
[GHC.Base](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/src/GHC-Base.html)
that don't show up in the documentation contents.

## Development tools

* There is a
  [Haskell mode](http://projects.haskell.org/haskellmode-emacs/) for
  emacs, but you have to install it separately.  Some OSes have a
  package for it, some don't (in which case you can put it in your
  come directory).  David uses the following configuration is his
  `.emacs` file:

    ~~~~
    (add-to-list 'completion-ignored-extensions ".hi")
    (or (fboundp 'haskell-mode)
        (let ((paths
    	   '("~/haskell-mode-2.8/haskell-site-file"
    	     "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")))
          (while paths
    	(if (not (file-exists-p (concat (car paths) ".el")))
    	    (setq paths (cdr paths))
    	  (load (car paths) t t)
    	  (setq paths nil)))))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook (lambda () (require 'inf-haskell)))
    ~~~~

    It is convenient to run GHCI within emacs.  Typing `C-c C-l` while
    editing a Haskell source file starts GHCI and loads the current
    file.


## Outside places to get help with Haskell

* The
[#haskell IRC channel](http://www.haskell.org/haskellwiki/IRC_channel)
contains many people willing to discuss Haskell and answer questions.

* [Haskell Cafe](http://www.haskell.org/mailman/listinfo/haskell-cafe)
is a mailing list for general Haskell-related discussion.

* A several Haskell experts regularly answer questions on
[Stack Overflow](http://stackoverflow.com/questions/tagged/haskell).


[Platform]: http://hackage.haskell.org/platform/
