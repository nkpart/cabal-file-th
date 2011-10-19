{-# LANGUAGE TemplateHaskell #-}
module Main where

import Distribution.PackageDescription.TH

main = do
  putStrLn $ "This package is version " ++ $(packageVariable (pkgVersion . package))
  let testVersion = $(packageVariableFrom "test/test-version-interp.cabal" (pkgVersion . package))
  let expectedVersion = "5.5.5"
  if testVersion /= expectedVersion
    then error ("Expected " ++ expectedVersion ++ ", read: " ++ testVersion)
    else putStrLn "Everything went better than expected."
