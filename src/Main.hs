import System.IO (openFile, hGetContents')
import System.Directory.Internal.Prelude (getArgs, IOMode (ReadMode))

import ElipticCurve 
import ECDSA
import InputParsers
import Text.Parsec (parse)


myCurve = ElipticCurve {
    p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F,
    a = 0,
    b = 7,
    g = Point{
        x=0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
        y=0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    },
    n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141,
    h = 1
}

-- Action definitions
outputElipticCurve :: String -> IO ()
outputElipticCurve "" = error "[Error]: No input provided!"
outputElipticCurve input = 
    case parse elipticCurveParser "" input of
        Left err -> print err 
        Right ans -> print ans


outputKeyPair :: String -> IO ()
outputKeyPair "" = error "[Error]: No input provided!"
outputKeyPair input = do
        putStrLn input
        putStrLn $ show $ KeyPair 0 $ Point 0 0 


outputSignature :: String -> IO ()
outputSignature "" = error "[Error]: No input provided!"
outputSignature input = do
        putStrLn input
        putStrLn $ show $ Signature 0 0


outputSignatureVerification :: String -> IO ()
outputSignatureVerification "" = error "[Error]: No input provided!"
outputSignatureVerification input = do
        putStrLn input
        putStrLn $ show $ True


actionMap :: [(String, String -> IO ())]
actionMap = [("-i", outputElipticCurve),
             ("-k", outputKeyPair),
             ("-s", outputSignature),
             ("-v", outputSignatureVerification)]


main :: IO ()
main = do
    (switch:inputSrc) <- getArgs
    
    let (Just action) = lookup switch actionMap
        readInput
            | null inputSrc = getContents
            | otherwise = do
                handle <- openFile (head inputSrc) ReadMode
                hGetContents' handle

    input <- readInput
    action input

