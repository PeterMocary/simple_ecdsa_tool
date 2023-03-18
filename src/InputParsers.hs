module InputParsers (
    elipticCurveParser
) where


import Control.Monad
import Numeric (readHex, readDec)
import Text.ParserCombinators.Parsec

import ECDSA
import ElipticCurve


pointField :: String -> Parser Point
pointField fieldName = do
    spaces
    string fieldName
    spaces
    char ':'
    spaces
    string "Point"
    spaces
    char '{'
    x <- numField "x" hexNum
    y <- numField "y" hexNum
    spaces
    char '}'
    return $ Point x y


numField :: (Num a) => String -> Parser a -> Parser a
numField fieldName contentsParser = do
    spaces
    string fieldName
    spaces
    char ':'
    contentsParser

hexNum :: Parser Integer
hexNum = do
    spaces
    string "0x"
    digits <- many1 hexDigit
    let ((d,_):_) = readHex digits
    return d

decNum :: Parser Integer
decNum = do
    spaces
    digits <- many1 digit
    let ((d,_):_) = readDec digits
    return d


elipticCurveParser :: Parser ElipticCurve
elipticCurveParser = do
    spaces
    string "Curve"
    spaces
    char '{'
    p <- numField "p" hexNum
    a <- numField "a" decNum
    b <- numField "b" decNum
    g <- pointField "g"
    n <- numField "n" hexNum
    h <- numField "h" decNum
    spaces
    char '}'
    return $ ElipticCurve p a b g n h

