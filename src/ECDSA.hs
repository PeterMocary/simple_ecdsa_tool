-- --------------------------------
-- FLP - ECDSA project
-- author: Peter Močáry (xmocar00)
-- date: 22.03.2023
-- --------------------------------

module ECDSA (
    Signature(..), KeyPair(..),
    pubKeyToStrSEC,
    generateKeyPair,
    generateSignature,
    verifySignature
) where

import Numeric (showHex)
import System.Random

import ElipticCurve (ElipticCurve(..), Point(..),
                     addPoints, multiplyPoint, multInv)


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

-- Normalizes length of a give hex integer in string format
-- to the length of given eliptic curve parameter p to
-- preserve leading zeroes.
_normalizeHexNumLength :: ElipticCurve -> String -> String
_normalizeHexNumLength (ElipticCurve p _ _ _ _ _) inputHex
    | pHexLen == inputHexLen = inputHex
    | otherwise = replicate lengthDiff '0' ++ inputHex
    where
        pHex = showHex p ""
        pHexLen = length pHex
        inputHexLen = length inputHex
        lengthDiff = pHexLen - inputHexLen


-- Converts public key to string SEC representation.
-- see https://secg.org/sec1-v2.pdf#subsubsection.2.3.3
pubKeyToStrSEC :: ElipticCurve -> Point -> String
pubKeyToStrSEC _ InfinityPoint = "0x00"
pubKeyToStrSEC ec q 
    | length output == 2*mlen = output
    | otherwise = replicate (2*mlen - length output) '0' ++ output
    where
        xHexStr = _normalizeHexNumLength ec $ showHex (x q) ""
        yHexStr = _normalizeHexNumLength ec $ showHex (y q) ""
        mlen = ceiling (logBase 2.0 (fromIntegral (p ec)) / 8)
        output = "0x04" ++ take (2*mlen) xHexStr ++ take (2*mlen) yHexStr


-- Generates a key pair.
-- Uses randomRIO to generate private key.
generateKeyPair :: ElipticCurve -> IO KeyPair
generateKeyPair ec@(ElipticCurve _ _ _ _ n _) = do
    privKey <- randomRIO (1, n-1)
    return $ _createKeyPair ec privKey


-- Creates key pair from a given private key and calculates
-- public key from the private key.
_createKeyPair :: ElipticCurve -> Integer -> KeyPair
_createKeyPair ec@(ElipticCurve p _ _ g _ _) privKey
    | privKey < 0 || privKey > p -1 = error "[InternalError]: Private Key out of range"
    | otherwise = KeyPair privKey pubKey
    where pubKey = multiplyPoint ec g privKey


-- Atempts to generate signature based on given random value (randomK). 
-- Intended to be called multiple times with a different random value until the
-- signature is generated successfully. Due to the uncertain nature of the generation process
-- uses Maybe as the return type.
_atemptToGenerateSignature :: ElipticCurve -> Integer -> Integer -> Integer -> Maybe Signature
_atemptToGenerateSignature ec@(ElipticCurve _ _ _ g n _) msgHash privKey randomK
    | r == 0 = Nothing
    | otherwise = Just $ Signature r s
    where
        kG = multiplyPoint ec g randomK
        r = x kG `mod` n
        s = (multInv randomK n * (msgHash + r * privKey)) `mod` n


-- Implements signature generation. Generates random number in the process.
generateSignature :: ElipticCurve -> Integer -> Integer -> IO Signature
generateSignature ec@(ElipticCurve _ _ _ _ n _) msgHash privateKey = do
    randK <- randomRIO (1, n-1)
    case _atemptToGenerateSignature ec msgHash privateKey randK of
        Nothing -> generateSignature ec msgHash privateKey
        Just signature -> return signature


-- Implements signature verification.
verifySignature :: ElipticCurve -> Signature -> Point -> Integer -> Bool
verifySignature _ _ InfinityPoint _ = False
verifySignature ec@(ElipticCurve _ _ _ g n _)  (Signature r s) pubKey msgHash
    | r > 0 && r < n && s > 0 && s < n = r == x newPoint
    | otherwise = False
    where
        w = multInv s n
        u1 = (w*msgHash) `mod` n
        u2 = (r*w) `mod` n
        newPoint1 = multiplyPoint ec g u1
        newPoint2 = multiplyPoint ec pubKey u2
        newPoint = addPoints ec newPoint1 newPoint2
