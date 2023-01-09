{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Distribution.PackageDescription.TH
import Control.Monad (when)
#if MIN_VERSION_Cabal(3,2,1)
import Distribution.Utils.ShortText
#endif

main :: IO ()
main = do
  let simpleAssert msg a b =
        when (a /= b) $
          error (msg ++ ": expected " ++ b ++ ", read: " ++ a)

  putStrLn $ "This package is version " ++ $(packageVariable (pkgVersion . package))
  let testVersion = $(packageVariableFrom "test/test-version-interp.cabal" (pkgVersion . package))
#if MIN_VERSION_Cabal(3,2,1)
      testCopyright = $(packageVariableFrom "test/test-version-interp.cabal" (packageString . fromShortText . copyright))
#else
      testCopyright = $(packageVariableFrom "test/test-version-interp.cabal" (packageString . copyright))
#endif
  simpleAssert "version" testVersion "5.5.5"
  simpleAssert "copyright" testCopyright "(c) 1953 Gumby"

  putStrLn "Everything went better than expected."

