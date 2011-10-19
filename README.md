
cabal-file-th
=============

Use template haskell to bring fields from your cabal file into your haskell source files.

Usage
-----

    import qualified Distribution.PackageDescription.TH as P

    myVersion :: String
    myVersion = $(packageVariable (pkgVersion . package))

Install
-------

    $ cabal install cabal-file-th
