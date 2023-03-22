-- --------------------------------
-- FLP - ECDSA project
-- author: Peter Močáry (xmocar00)
-- date: 22.03.2023
-- --------------------------------

module InputParsers (
    elipticCurveParser,
    signatureGenerationInputParser,
    signatureVerificationInputParser
) where

import Numeric (readHex)
import Text.Read (readMaybe)
import Text.ParserCombinators.Parsec

import ECDSA (KeyPair(..), Signature(..))
import ElipticCurve (ElipticCurve(..), Point(..))


_pointField :: String -> Parser Point
_pointField fieldName = do
    _ <- spaces
    _ <- string fieldName
    _ <- spaces
    _ <- char ':'
    _ <- spaces
    _ <- string "Point"
    _ <- spaces
    _ <- char '{'
    x <- _numField "x"
    y <- _numField "y"
    _ <- spaces
    _ <- char '}'
    return $ Point x y

-- Implements parsing of a numeric field with given filed name.
-- Can read both hexadecimal or decimal integers from the input.
_numField :: String -> Parser Integer
_numField fieldName = do
    _ <- spaces
    _ <- string fieldName
    _ <- spaces
    _ <- char ':'
    _ <- spaces
    digits <- manyTill anyChar newline
    case readMaybe digits :: Maybe Integer of
        Just d -> return d
        Nothing -> unexpected "character on previous line. Expected hexadecimal or decimal integer"

-- implements parsing of SEC format of a public Key
-- see https://secg.org/sec1-v2.pdf#subsubsection.2.3.3
_publicKeyField :: Parser Point
_publicKeyField = do
    _ <- spaces
    _ <- char 'Q'
    _ <- spaces
    _ <- char ':'
    _ <- spaces
    _ <- string "0x04"
    digits <- many1 hexDigit
    let ((firstHalf,_):_) = readHex $ take (length digits `div` 2) digits
        ((secondHalf,_):_) = readHex $ drop (length digits `div` 2) digits
    return $ Point firstHalf secondHalf


_signatureParser :: Parser Signature
_signatureParser = do
    _ <- spaces
    _ <-  string "Signature"
    _ <-  spaces
    _ <-  char '{'
    r <- _numField "r"
    s <- _numField "s"
    _ <- spaces
    _ <- char '}'
    return $ Signature r s


_publicKeyParser :: Parser Point
_publicKeyParser = do
    _ <- spaces
    _ <- string "PublicKey"
    _ <- spaces
    _ <- char '{'
    publicKey <- _publicKeyField
    _ <- spaces
    _ <- char '}'
    return publicKey


_keyPairParser :: Parser KeyPair
_keyPairParser = do
    _ <- spaces
    _ <- string "Key"
    _ <- spaces
    _ <- char '{'
    privateKey <- _numField "d"
    publicKey <- _publicKeyField
    _ <- spaces
    _ <- char '}'
    return $ KeyPair privateKey publicKey


_messageHashParser :: Parser Integer
_messageHashParser = _numField "Hash"


-- implements parser for the input of -i and -k switches
elipticCurveParser :: Parser ElipticCurve
elipticCurveParser = do
    _ <- spaces
    _ <- string "Curve"
    _ <- spaces
    _ <- char '{'
    p <- _numField "p"
    a <- _numField "a"
    b <- _numField "b"
    g <- _pointField "g"
    n <- _numField "n"
    h <- _numField "h"
    _ <- spaces
    _ <- char '}'
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
