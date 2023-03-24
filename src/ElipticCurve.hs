-- --------------------------------
-- FLP - ECDSA project
-- author: Peter Močáry (xmocar00)
-- date: 22.03.2023
-- --------------------------------

module ElipticCurve (
    ElipticCurve(..), Point(..),
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
    show ec = "Curve {" ++
              "\np: 0x" ++ showHex (p ec) "" ++
              "\na: " ++ show (a ec) ++ 
              "\nb: " ++ show (b ec) ++ 
              "\ng: " ++ show (g ec) ++ 
              "\nn: 0x" ++ showHex (n ec) "" ++
              "\nh: " ++ show (h ec) ++ "\n}"


data Point = Point {
    x :: Integer,
    y :: Integer
} | InfinityPoint deriving(Eq)

instance Show Point where
    show InfinityPoint = "Point {" ++
                         "\n    x: 0x00" ++
                         "\n    y: 0x00\n}"
    show point = "Point {" ++
                 "\n    x: 0x" ++ showHex (x point) "" ++
                 "\n    y: 0x" ++ showHex (y point) "\n}"


-- Implemnts multiplicative inverse of a number 'a' with moudlus 'm'
-- uses small fermat theorem, thus expects 'm' to be prime
multInv :: Integer -> Integer -> Integer
multInv base modulus = _powMod base (modulus - 2) modulus


-- Implements faster way of calculating x^n mod m
_powMod :: Integer -> Integer -> Integer -> Integer
_powMod _ 0 _ = 1
_powMod base expon modulus
    | even expon = (base' * base') `mod` modulus
    | otherwise = (base * base' * base') `mod` modulus
    where
        base' = _powMod base (expon `div` 2) modulus


-- Checks if given points are inverse to each other.
_isInversePointTo :: ElipticCurve -> Point -> Point -> Bool
_isInversePointTo _ InfinityPoint InfinityPoint = True
_isInversePointTo _ _ InfinityPoint = False
_isInversePointTo _ InfinityPoint _ = False
_isInversePointTo ec (Point _ yA) (Point _ yB) = yA == -yB `mod` p ec


-- Inverts the specified eliptic curve point
_getInversePointTo :: ElipticCurve -> Point -> Point
_getInversePointTo _ InfinityPoint = InfinityPoint
_getInversePointTo ec point = Point (x point) $ -(y point) `mod` p ec


-- Implements addition of two eliptic curve points
addPoints :: ElipticCurve -> Point -> Point -> Point                        -- P + Q = R
addPoints _ InfinityPoint point = point                                    -- P==O                   => O + Q = Q
addPoints _ point InfinityPoint = point                                    -- Q==O                   => P + O = P
addPoints ec pointA@(Point xA yA) pointB@(Point xB yB)
    | _isInversePointTo ec pointA pointB = InfinityPoint                     -- Q==-P                  => P + (-P) = O
    | pointA == pointB && yA == 0 = InfinityPoint                           -- P==Q and y=0           => P + P = O
    | pointA == pointB = Point xR_eq yR_eq                                  -- P==Q                   => P + P = R
    | otherwise = Point xR yR                                               -- P /= Q && -P /= Q      => P + Q = R
    where
        -- P == Q
        s_eq = (3*(xA*xA) + a ec) * multInv (2*yA) (p ec) `mod` p ec
        xR_eq = (s_eq*s_eq - 2 * xA) `mod` p ec
        yR_eq = (s_eq * (xA - xR_eq) - yA) `mod` p ec
        -- P /= Q
        s = (yA - yB) * multInv (xA - xB) (p ec) `mod` p ec
        xR = (s*s - xA - xB) `mod` p ec
        yR = (s * (xA - xR) - yA) `mod` p ec


-- Implements multiplication of an eliptic curve point by a constant
multiplyPoint :: ElipticCurve -> Point -> Integer -> Point
multiplyPoint _ point 1 = point
multiplyPoint ec point constant 
    | even constant = addPoints ec point' point'
    | otherwise = addPoints ec point $ addPoints ec point' point'
    where point' = multiplyPoint ec point $ constant `div` 2
