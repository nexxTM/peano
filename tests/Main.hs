module Main where

import Peano

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, Arbitrary, arbitrary, shrink, choose, (==>))

import Data.Functor ((<$>))

instance Arbitrary Nat where
    arbitrary = toEnum <$> (choose (0,42))
    shrink (Succ n) = [n]
    shrink Zero = []

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Peano" [
            testProperty "Reflexiv" $ peanoReflexive,
            testProperty "Symmetric" $ peanoSymmetric,
            testProperty "Transitive" $ peanoTransitive
        ]
    ]

peanoReflexive :: Nat -> Bool
peanoReflexive n = n == n

peanoSymmetric :: Nat -> Nat -> Property
peanoSymmetric n m = n == m ==> m == n

peanoTransitive :: Nat -> Nat -> Nat -> Property
peanoTransitive n m o = n == m && m == o ==> n == o
