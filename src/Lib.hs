{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing#-}
module Lib where

import Data.Sequence (Seq, pattern (:<|), pattern (:|>))
import Data.Sequence qualified as Seq

import Data.List qualified as List

import Data.Foldable (toList)

import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype Poly a = UncheckedMkPoly (Seq a)

instance (Show a, Eq a, Num a) => Show (Poly a) where
    show (UncheckedMkPoly []) = "0"
    show (UncheckedMkPoly coeffs) =
        List.intercalate " + " (reverse (toList (Seq.mapWithIndex showCoefficient coeffs)))
        where
            showCoefficient 0 c = show c
            showCoefficient 1 1 = "X"
            showCoefficient 1 c = show c <> "*X"
            showCoefficient n 1 = "X^" <> show n
            showCoefficient n c = show c <> "*X^" <> show n

-- poly [a,b,c] ~= a + bx + cx²
poly :: (Num a, Eq a) => Seq a -> Poly a
poly seq = UncheckedMkPoly (Seq.dropWhileR (== 0) seq)

coefficients :: Poly a -> Seq a
coefficients (UncheckedMkPoly poly) = poly

coefficientAt :: Num a => Int -> Poly a -> a
coefficientAt index poly = case (coefficients poly) Seq.!? index of
    Nothing -> 0
    Just x -> x

highestCoefficient :: Num a => Poly a -> a
highestCoefficient (UncheckedMkPoly Seq.Empty) = 0
highestCoefficient (UncheckedMkPoly (_ Seq.:|> highest)) = highest

degree :: Poly a -> Int
degree poly1 = Seq.length (coefficients poly1)

mapPoly :: (Num b, Eq b) => (a -> b) -> Poly a -> Poly b
mapPoly f (UncheckedMkPoly poly1) = poly (fmap f poly1) 

zipPoly :: (Num b, Eq b) => (a -> a -> b) -> (a -> b) -> Poly a -> Poly a -> Poly b
zipPoly zipBoth zipOne (UncheckedMkPoly poly1) (UncheckedMkPoly poly2) =
    poly $ Seq.zipWith zipBoth poly1 poly2 <> (fmap zipOne (Seq.drop (length poly1) poly2 <> Seq.drop (length poly2) poly1))

shiftCoefficients :: (Num a, Eq a) => Int -> Poly a -> Poly a
shiftCoefficients n (UncheckedMkPoly coeffs) =
    poly (Seq.replicate n 0 <> coeffs)

instance (Num a, Eq a) => Num (Poly a) where
    (+) = zipPoly (+) id
    negate = mapPoly negate
    fromInteger n = poly [fromInteger n]
    poly1 * poly2
        | degree poly2 < degree poly1 = poly2 * poly1
        | otherwise =
            sum (Seq.mapWithIndex (\i c -> shiftCoefficients i (mapPoly (* c) poly2)) (coefficients poly1))

divide :: (Fractional a, Eq a) => Poly a -> Poly a -> (Poly a, Poly a)
divide poly1 poly2
    | degree poly1 < degree poly2 = (0, poly1)
    | otherwise =  do
        let shift = degree poly1 - degree poly2
        let factor = highestCoefficient poly1 / highestCoefficient poly2
        
        let component = shiftCoefficients shift (poly [factor])
        
        let toSubtract = component * poly2

        let (remaining, rest) = divide (poly1 - toSubtract) poly2

        (component + remaining, rest)

divideFloor :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
divideFloor poly1 poly2 = fst $ divide poly1 poly2

pattern X :: Num a => () => Poly a
pattern X <- undefined
    where
        X = UncheckedMkPoly [0, 1]


data Expr a
    = Polynomial (Poly a)
    | Add (Seq (Expr a))
    | Multiply (Seq (Expr a))
    | Negate (Expr a)
    | Invert (Expr a)

instance (Num a, Eq a) => Num (Expr a) where
    fromInteger x = Polynomial (fromInteger x)

    Polynomial x + Polynomial y = Polynomial (x + y)
    Add (xs :|> Polynomial x) + Polynomial y = Add (xs :|> Polynomial (x + y))
    Polynomial x + Add (Polynomial y :<| ys) = Add (Polynomial (x + y) :<| ys)
    Add (xs :|> Polynomial x) + Add (Polynomial y :<| ys) = Add (xs <> [Polynomial (x + y)] <> ys)
    Add xs + Add ys = Add (xs <> ys)
    Add xs + y = Add (xs :|> y)
    x + Add ys = Add (x :<| ys)
    x + y = Add [x, y]

    Multiply xs * Multiply ys = Multiply (xs <> ys)
    Multiply xs * y = Multiply (xs <> [y])
    x * Multiply ys = Multiply ([x] <> ys)
    x * y = Multiply [x, y]

    negate (Negate x) = x
    negate x = Negate x

    abs = undefined
    signum = undefined

decomposeRatio :: (Floating a, Eq a) => Poly a -> Poly a -> Expr a
decomposeRatio enumerator divisor
    | degree enumerator > degree divisor = do
        let (wholePoly, rest) = enumerator `divide` divisor
        Polynomial wholePoly + decomposeRatio rest divisor
    | Just roots <- findRoots divisor = do
        undefined
    | otherwise = do
        undefined

-- | Find obvious roots of a given polynomial
findRoots :: (Floating a, Eq a) => Poly a -> Maybe (Seq a)
findRoots poly | coefficientAt 0 poly == 0 = (0 :<|) <$> findRoots (divideFloor poly X)
findRoots poly = case coefficients poly of
    [] -> Just []
    [x] -> Just []
    [x, y] -> Just [-x / y]
    [x, y, z] -> do
        let p = y/x
        let q = z/x
        Just [-(p/2) + sqrt((p/2)^2 - q), -(p/2) + sqrt((p/2)^2 + q)]
    _ -> Nothing
