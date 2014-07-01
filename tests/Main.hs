module Main where

import Peano

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, Arbitrary, arbitrary, shrink, choose, (==>))

import Data.Monoid (mempty, mappend)
import Data.Functor ((<$>))

instance Arbitrary Nat where
    arbitrary = toEnum <$> (choose (0,42))
    shrink (Succ n) = [n]
    shrink Zero = []

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "Peano Equality" [
            testProperty "Reflexive" $ peanoReflexive,
            testProperty "Symmetric" $ peanoSymmetric,
            testProperty "Transitive" $ peanoTransitive
        ],
        testGroup "Peano Enum" [
            testProperty "fromTo" $ peanoEnumFromTo
        ],
        testGroup "Peano Monoid" [
            testProperty "Left identity" $ peanoMonoidLeft,
            testProperty "Right identity" $ peanoMonoidRight,
            testProperty "Associative" $ peanoMonoidAssociative
        ]
    ]

peanoReflexive :: Nat -> Bool
peanoReflexive n = n == n

peanoSymmetric :: Nat -> Nat -> Property
peanoSymmetric n m = n == m ==> m == n

peanoTransitive :: Nat -> Nat -> Nat -> Property
peanoTransitive n m o = n == m && m == o ==> n == o

peanoEnumFromTo :: Nat -> Bool
peanoEnumFromTo n = toEnum (fromEnum n) == n

peanoMonoidLeft :: Nat -> Bool
peanoMonoidLeft n = mempty `mappend` n == n

peanoMonoidRight :: Nat -> Bool
peanoMonoidRight n = n`mappend` mempty  == n

peanoMonoidAssociative :: Nat -> Nat -> Nat -> Bool
peanoMonoidAssociative n m o = (m `mappend` n) `mappend` o == m `mappend` (n `mappend` o)
