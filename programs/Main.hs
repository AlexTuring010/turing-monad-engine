module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Parser (parseProgram, isInAlphabet)
import qualified Map as MyMap
import Tokenizer (tokenize)
import Emulator (runEmulator)
import Types (AlphabetMap, Program, globalAlphabet)
import Data.Char (isSpace)

-- | Remove leading/trailing whitespace
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

validateInput :: AlphabetMap -> String -> Either String String
validateInput alphabetMap s =
    let symbols = words s
    in if "_" `elem` symbols
        then Left "Error: User input cannot contain blank symbol '_'"
    else case validateSymbols alphabetMap symbols of
        Left err -> Left err
        Right _ -> Right s
  where
    validateSymbols _ [] = Right ()
    validateSymbols alphabet (sym:rest) =
        if isInAlphabet sym alphabet
            then validateSymbols alphabet rest
            else Left ("Error: Symbol '" ++ sym ++ "' is not in the defined alphabet")

runParserPipeline :: String -> Either String Program
runParserPipeline fileContent = do
    tokens <- tokenize fileContent
    parseProgram tokens

getUserInput :: AlphabetMap -> IO (Either String String)
getUserInput alphabetMap = do
    let symbols = MyMap.mapKeys alphabetMap
    let alphabetStr = unwords symbols
    let prompt = "Enter initial tape content (symbols separated by spaces).\nValid alphabet: " ++ alphabetStr ++ ""
    putStrLn prompt
    rawInput <- getLine
    let input = trim rawInput
    return (validateInput alphabetMap input)

formatOutput :: (String, [String]) -> String
formatOutput (haltingState, finalTape) =
    "Final tape (including blanks): " ++ unwords finalTape ++ 
       "\nTermination state: " ++ haltingState

-- | Coordinator function that orchestrates the pipeline
coordinate :: String -> IO (Either String String)
coordinate filePath = do
    fileContent <- readFile filePath
    case runParserPipeline fileContent of
        Left err -> return (Left err)
        Right program -> do
            inputResult <- getUserInput (globalAlphabet program)
            case inputResult of
                Left err -> return (Left err)
                Right validInput -> do
                    let initialTape = "_" : words validInput
                    let result = runEmulator program initialTape
                    return (fmap formatOutput result)

-- | Main entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn ("Usage: turing-monad <program.turing>")
            exitFailure
        (filePath:_) -> do
            result <- coordinate filePath
            case result of
                Left err -> do
                    putStrLn err
                    exitFailure
                Right finalTape -> putStrLn finalTape