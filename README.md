# StaRVOOrS-tool

In order to work with the source code you have to install the following packages:

 language-java >= 0.2.7
 
 haxml >= 1.25.3 

 split >= 0.2.2


To install them, you must have a Haskell compiler: ghc-6.7 or later recommended

Then, for more recent compilers, use the standard Cabal method of installation:

    cabal update
    cabal install haxml

or
    runhaskell Setup.hs configure [--prefix=...] [--buildwith=...]
    runhaskell Setup.hs build
    runhaskell Setup.hs install

For older compilers, use:

    ./configure [--prefix=...] [--buildwith=...]
    make
    make install
