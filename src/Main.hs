import System.Environment (getArgs)
import Text.Parsec (parse)
import Numeric (showHex)

import ECDSA (KeyPair(..), generateKeyPair, pubKeyToStrSEC,
              Signature(..), generateSignature, verifySignature)
import InputParsers (elipticCurveParser,
                     signatureGenerationInputParser,
                     signatureVerificationInputParser)


-- Implements -i switch
outputElipticCurve :: String -> FilePath -> IO ()
outputElipticCurve "" _ = error "[Error]: No input provided!"
outputElipticCurve input filePath =
    case parse elipticCurveParser filePath input of
        Left err -> error $ show err
        Right elipticCurve -> putStrLn $ show elipticCurve


-- Implements -k switch
outputKeyPair :: String -> FilePath -> IO ()
outputKeyPair "" _ = error "[Error]: No input provided!"
outputKeyPair input filePath =
    case parse elipticCurveParser filePath input of
        Left err -> error $ show err
        Right elipticCurve -> do
            (KeyPair privateKey publicKey) <- generateKeyPair elipticCurve
            putStrLn $ "Key {\n" ++
                       "d: 0x" ++ showHex privateKey "\n" ++
                       "Q: " ++ pubKeyToStrSEC elipticCurve publicKey ++
                       "\n}"


-- Implements -s switch
outputSignature :: String -> FilePath -> IO ()
outputSignature "" _ = error "[Error]: No input provided!"
outputSignature input filePath =
    case parse signatureGenerationInputParser filePath input of
        Left err -> error $ show err
        Right (elipticCurve, KeyPair privateKey _, msgHash) -> do
            sig <- generateSignature elipticCurve msgHash privateKey
            putStrLn $ "Signature {" ++
                     "\nr: 0x" ++ showHex (r sig) "" ++
                     "\ns: 0x" ++ showHex (s sig) "" ++
                     "\n}"


-- Implements -v switch
outputSignatureVerification :: String -> FilePath -> IO ()
outputSignatureVerification "" _ = error "[Error]: No input provided!"
outputSignatureVerification input filePath =
    case parse signatureVerificationInputParser filePath input of
        Left err -> error $ show err
        Right (elipticCurve, signature, publicKey, msgHash) -> do
            print $ verifySignature elipticCurve signature publicKey msgHash


-- Action map binds switches to their logic
actionMap :: [(String, String -> FilePath -> IO ())]
actionMap = [("-i", outputElipticCurve),
             ("-k", outputKeyPair),
             ("-s", outputSignature),
             ("-v", outputSignatureVerification)]


main :: IO ()
main = do
    (switch:inputArgs) <- getArgs

    -- Decide whether to read from STDIN or from a file
    let _readInput :: (FilePath, IO String)
        _readInput
            | null inputArgs = ("STDIN", getContents)
            | otherwise = (head inputArgs, readFile $ head inputArgs)
    input <- snd _readInput

    -- Lookup and execute switch logic
    case lookup switch actionMap of
        Nothing -> error "[Error]: Undefined switch!"
        (Just action) -> action input $ fst _readInput

