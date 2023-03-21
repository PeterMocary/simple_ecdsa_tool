module InputParsers (
    elipticCurveParser,
    signatureGenerationInputParser,
    signatureVerificationInputParser
) where

import Numeric (readHex, readDec)
import Text.ParserCombinators.Parsec

import ECDSA
import ElipticCurve


_pointField :: String -> Parser Point
_pointField fieldName = do
    spaces
    string fieldName
    spaces
    char ':'
    spaces
    string "Point"
    spaces
    char '{'
    x <- _numField "x" _hexNum
    y <- _numField "y" _hexNum
    spaces
    char '}'
    return $ Point x y


_numField :: (Num a) => String -> Parser a -> Parser a
_numField fieldName contentsParser = do
    spaces
    string fieldName
    spaces
    char ':'
    contentsParser


_hexNum :: Parser Integer
_hexNum = do
    spaces
    string "0x"
    digits <- many1 hexDigit
    let ((d,_):_) = readHex digits
    return d


_decNum :: Parser Integer
_decNum = do
    spaces
    digits <- many1 digit
    let ((d,_):_) = readDec digits
    return d


_publicKeyField :: Parser Point
_publicKeyField = do
    spaces
    char 'Q'
    spaces
    char ':'
    spaces
    string "0x04"
    digits <- many1 hexDigit
    let ((firstHalf,_):_) = readHex $ take (length digits `div` 2) digits
        ((secondHalf,_):_) = readHex $ drop (length digits `div` 2) digits
    return $ Point firstHalf secondHalf


_signatureParser :: Parser Signature
_signatureParser = do
    spaces
    string "Signature"
    spaces
    char '{'
    r <- _numField "r" _hexNum
    s <- _numField "s" _hexNum
    spaces
    char '}'
    return $ Signature r s


_publicKeyParser :: Parser Point
_publicKeyParser = do
    spaces
    string "PublicKey"
    spaces
    char '{'
    publicKey <- _publicKeyField
    spaces
    char '}'
    return publicKey


_keyPairParser :: Parser KeyPair
_keyPairParser = do
    spaces
    string "Key"
    spaces
    char '{'
    privateKey <- _numField "d" _hexNum
    publicKey <- _publicKeyField
    spaces
    char '}'
    return $ KeyPair privateKey publicKey


_messageHashParser :: Parser Integer
_messageHashParser = _numField "Hash" _hexNum


-- implements parser for the input of -i and -k switches
elipticCurveParser :: Parser ElipticCurve
elipticCurveParser = do
    spaces
    string "Curve"
    spaces
    char '{'
    p <- _numField "p" _hexNum
    a <- _numField "a" _decNum
    b <- _numField "b" _decNum
    g <- _pointField "g"
    n <- _numField "n" _hexNum
    h <- _numField "h" _decNum
    spaces
    char '}'
    return $ ElipticCurve p a b g n h


-- Implements parser for the input of -s switch
signatureGenerationInputParser :: Parser (ElipticCurve, KeyPair, Integer)
signatureGenerationInputParser = do
    elipticCurve <- elipticCurveParser
    keyPair <- _keyPairParser
    msgHash <- _messageHashParser
    return (elipticCurve, keyPair, msgHash)


-- Implements parser for the input of -v switch
signatureVerificationInputParser :: Parser (ElipticCurve, Signature, Point, Integer)
signatureVerificationInputParser = do
    elipticCurve <- elipticCurveParser
    signature <- _signatureParser
    publicKey <- _publicKeyParser
    msgHash <- _messageHashParser
    return (elipticCurve, signature, publicKey, msgHash)
