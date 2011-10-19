{-# LANGUAGE TemplateHaskell #-}
module Main where

import Distribution.PackageDescription.TH

main = do
  putStrLn $(packageVar (pkgVersion . package))

