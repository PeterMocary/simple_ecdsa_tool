module ElipticCurve (
    ElipticCurve(..), Point(..),
    isInversePointTo,
    getInversePointTo,
    addPoints, multiplyPoint,
    multInv
) where

import Numeric (showHex)


data ElipticCurve = ElipticCurve {
    p :: Integer,
    a :: Integer,
    b :: Integer,
    g :: Point,
    n :: Integer,
    h :: Integer
}

instance Show ElipticCurve where 
    show (ElipticCurve p a b g n h) = "Curve: {" ++
                                      "\np: 0x" ++ showHex p "" ++
                                      "\na: " ++ show a ++ 
                                      "\nb: " ++ show b ++ 
                                      "\ng: " ++ show g ++ 
                                      "\nn: 0x" ++ showHex n "" ++
                                      "\nh: " ++ show h ++ "\n}"


data Point = Point {
    x :: Integer,
    y :: Integer
} | InfinityPoint deriving(Eq)

instance Show Point where
    show (Point x y) = "Point {" ++
                       "\n    x: 0x" ++ showHex x "" ++
                       "\n    y: 0x" ++ showHex y "\n}"
    show InfinityPoint = "Point {" ++
                         "\n    x: 0x00" ++
                         "\n    y: 0x00\n}"


-- Implemnts multiplicative inverse of a number 'a' with moudlus 'm'
-- uses small fermat theorem, thus expects 'm' to be prime
multInv :: Integer -> Integer -> Integer
multInv a m = _powMod a (m - 2) m


-- Implements faster way of calculating x^n mod m
_powMod :: Integer -> Integer -> Integer -> Integer
_powMod _ 0 _ = 1
_powMod x n m
    | even n = (x' * x') `mod` m
    | otherwise = (x * x' * x') `mod` m
    where
        x' = _powMod x (n `div` 2) m


-- Checks if the points are inverse to each other
isInversePointTo :: ElipticCurve -> Point -> Point -> Bool
isInversePointTo _ InfinityPoint InfinityPoint = True
isInversePointTo _ _ InfinityPoint = False
isInversePointTo _ InfinityPoint _ = False
isInversePointTo (ElipticCurve p _ _ _ _ _) (Point _ yA) (Point _ yB) = yA == -yB `mod` p


-- Inverts the specified eliptic curve point
getInversePointTo :: ElipticCurve -> Point -> Point
getInversePointTo _ InfinityPoint = InfinityPoint
getInversePointTo (ElipticCurve p _ _ _ _ _) (Point x y) = Point x $ -y `mod` p


-- Implements addition of two eliptic curve points
addPoints :: ElipticCurve -> Point -> Point -> Point                        -- P + Q = R
addPoints _ InfinityPoint point = point                                    -- P==O                   => O + Q = Q
addPoints _ point InfinityPoint = point                                    -- Q==O                   => P + O = P
addPoints ec@(ElipticCurve p a _ _ _ _) pointA@(Point xA yA) pointB@(Point xB yB)
    | isInversePointTo ec pointA pointB = InfinityPoint                     -- Q==-P                  => P + (-P) = O
    | pointA == pointB && yA == 0 = InfinityPoint                           -- P==Q and y=0           => P + P = O
    | pointA == pointB = Point xR_eq yR_eq                                  -- P==Q                   => P + P = R
    | otherwise = Point xR yR                                               -- P /= Q && -P /= Q      => P + Q = R
    where
        -- P == Q
        s_eq = (3*(xA^2) + a) * multInv (2*yA) p `mod` p
        xR_eq = (s_eq^2 - 2 * xA) `mod` p
        yR_eq = (s_eq * (xA - xR_eq) - yA) `mod` p
        -- P /= Q
        s = (yA - yB) * multInv (xA - xB) p `mod` p
        xR = (s^2 - xA - xB) `mod` p
        yR = (s * (xA - xR) - yA) `mod` p


-- Implements multiplication of an eliptic curve point by a constant
multiplyPoint :: ElipticCurve -> Point -> Integer -> Point
multiplyPoint _ point 1 = point
multiplyPoint ec point constant 
    | even constant = addPoints ec point' point'
    | otherwise = addPoints ec point $ addPoints ec point' point'
    where point' = multiplyPoint ec point $ constant `div` 2

