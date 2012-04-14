Haskell bindings for libgpod
============================

[libgpod](http://www.gtkpod.org/wiki/Libgpod) is a popular library to manage the
music library on Apple devices such as the ubiquitous iPod.

This project provides [Haskell](http://www.haskell.org/) bindings to this
library, so that you can manipulate iTunes music libraries in Haskell programs.

Why ?
-----

By increasing amount of importance :

  - because [libgpod](http://www.gtkpod.org/wiki/Libgpod) is a popular library
    and does not have Haskell bindings.
  - so I can learn how to use the
    [Foreign Function Interface](http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi.html).
  - so that I can implement an `ipod://` special remote for
    [git-annex](http://git-annex.branchable.com/).
