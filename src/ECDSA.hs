module ECDSA (
    Signature(..), KeyPair(..),
    createKeyPair,
    pubKeyToStrSEC,
    atemptToGenerateSignature,
    verifySignature
) where

import Numeric (showHex)
import ElipticCurve

data Signature = Signature {
    r :: Integer,
    s :: Integer
}

instance Show Signature where
    show (Signature r s) = "Signature {" ++
                           "\nr: 0x" ++ showHex r "" ++
                           "\ns: 0x" ++ showHex s "\n}"

data KeyPair = KeyPair {
    d :: Integer,
    q :: Point
}

instance Show KeyPair where
    show (KeyPair d q) = "Key {" ++
                         "\nd: 0x" ++ showHex d "" ++
                         "\nQ: \n" ++ show q ++ "\n}"


-- Creates key pair from given private key.
-- Calculates public key from the private key.
createKeyPair :: ElipticCurve -> Integer -> KeyPair
createKeyPair ec@(ElipticCurve p _ _ g _ _) privKey
    | privKey < 0 || privKey > p -1 = error "[InternalError]: Private Key out of range"
    | otherwise = KeyPair privKey pubKey
    where
        pubKey = multiplyPoint ec g privKey

-- Converts public key to string SEC representation.
-- see https://secg.org/sec1-v2.pdf#subsubsection.2.3.3
pubKeyToStrSEC:: ElipticCurve -> Point -> String
pubKeyToStrSEC ec InfinityPoint = "0x00"
pubKeyToStrSEC ec q 
    | length output == 2*mlen = output
    | otherwise = replicate (2*mlen - length output) '0' ++ output
    where
        xHexStr = showHex (x q) ""
        yHexStr = showHex (y q) ""
        mlen = ceiling (logBase 2.0 (fromIntegral (p ec)) / 8)
        output = "0x04" ++ take (2*mlen) xHexStr ++ take (2*mlen) yHexStr

-- Implements signature generation.
-- Atempts to generate signature based on given random value (randomK). 
-- Intended to be called multiple times with a different random value until the
-- signature is generated successfully.
atemptToGenerateSignature :: ElipticCurve -> Integer -> Integer -> Integer -> Maybe Signature
atemptToGenerateSignature ec@(ElipticCurve _ _ _ g n _) msgHash privKey randomK
    | r == 0 = Nothing
    | otherwise = Just $ Signature r s
    where
        kG = multiplyPoint ec g randomK
        r = x kG `mod` n
        s = (multInv randomK n * (msgHash + r * privKey)) `mod` n

-- Implements signature verification.
-- Doesn't check corectness of given eliptic curve and public key.
verifySignature :: ElipticCurve -> Signature -> Point -> Integer -> Bool
verifySignature _ _ InfinityPoint _ = False
verifySignature ec@(ElipticCurve _ _ _ g n _)  (Signature r s) pubKey msgHash
    | r > 0 && r < n && s > 0 && s < n = r == x newPoint
    | otherwise = False
    where
        w = multInv s n `mod` n
        u1 = w*msgHash `mod` n
        u2 = r*w `mod` n
        newPoint1 = multiplyPoint ec g u1
        newPoint2 = multiplyPoint ec pubKey u2
        newPoint = addPoints ec newPoint1 newPoint2
