module Main ( main ) where
import Distribution.Simple.Test.LibV09 ( stubMain )
import Test ( tests )
main :: IO ()
main = stubMain tests
