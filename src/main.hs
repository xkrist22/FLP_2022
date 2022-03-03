-- Author: Jiri Kristof <xkrist22@stud.fit.vutbr.cz>
-- Date: February 2022
-- File: main.hs

import qualified System.Environment as Env
import qualified Parser
import qualified Easy
import qualified Cnf


-- Main function
main :: IO Int
main = do
	-- Parse cmd arguments
	args <- Env.getArgs
	let progSettings = parseArgs args

	-- TODO: implement reading of input
	{--
	let nonParsedCfg
		| snd progSettings == "stdout" = readCfgFromStdout
		| otherwise = readCfgFromFile (snd progSettings)
	--}

	let nonParsedCfg = removeWhitespace "A,B,S\na,b\nS\nS->AB\nA->a\nB->b"

	-- get result based on selected mode
	let result
		| getMode progSettings == 0 = runMode0 nonParsedCfg
		| getMode progSettings == 1 = runMode1 nonParsedCfg
		| getMode progSettings == 2 = runMode2 nonParsedCfg
		-- note that this should be unreachable
		| otherwise = error "Unknown program mode"

	return result


-- Function parse arguments from cmd
-- Params:
-- 	args: list containing arguments given from terminal
-- Returns: tuple, where first element is mod of program
-- 	second element contains file where CFG is stored (or stdout) 
-- Note: if arguments are invalid, then second element of return
-- 	tuple contain error describtion
parseArgs :: [String] -> (Int, String) 	
parseArgs args
	-- no args given
	| (length args == 0) = (2, "stdout")
	-- arg -i given
	| (length args == 1 && "-i" `elem` args) = (0, "stdout")
	-- arg -1 given
	| (length args == 1 && "-1" `elem` args) = (1, "stdout")
	--arg -2 given
	| (length args == 1 && "-2" `elem` args) = (2, "stdout")
	--arg <filename> given
	| (length args == 1) = (2, args !! 0)
	-- args -i <filename> given
	| (length args == 2 && args !! 0 == "-i") = (0, args !! 1)
	-- args -1 <filename> given
	| (length args == 2 && args !! 0 == "-1") = (1, args !! 1)
	-- args -2 <filename> given
	| (length args == 2 && args !! 0 == "-2") = (2, args !! 1)
	| otherwise = error "Input parameters error"


-- Function removes space and tab from any string
-- Params:
-- 	string: any string from which spaces and tabs will be removed
-- Returns: string without spaces and tabs
removeWhitespace :: String -> String
removeWhitespace (symbol:symbolList) 
	| symbol == ' ' = removeWhitespace symbolList
	| symbol == '\t' = removeWhitespace symbolList
	| otherwise = symbol : (removeWhitespace symbolList) 
removeWhitespace [] = []


-- Getter for mode of program
-- Params:
-- 	progSettings: tuple containing info about desired mode of
-- 		program and source of scf
-- Returns: desired mode of program 
getMode :: (Int, String) -> Int
getMode (mode, _) = mode 
 

-- Getter for source of input CFG
-- Params:
-- 	progSettings: tuple containing info about desired mode of
-- 		program and source of cfg
-- Returns: source of cfg 
getCfgSource :: (Int, String) -> String 
getCfgSource (_, source) = source 
 

-- Function reads input CFG from stdout
-- Returns: string containing input CFG
{--
readCfgFromStdout = do
	return ["nonterminal", "terminal", "starting", "rule1", "rule2"]


-- Function reads input CFG from given filename
-- Params:
-- 	filename: string containing name of the file where CFG is stored
-- Returns: string containing input CFG	
readCfgFromFile filename = do
	return ["nonterminal", "terminal", "starting", "rule1", "rule2"]
----}

getCommaSequence [] = ""
getCommaSequence (last:[]) = (show last) ++ "\n" 
getCommaSequence (elem:list) = (show last) ++ "," ++ (getCommaSequence list)


getRuleSequence [] = ""
getRuleSequence (last:[]) = (show (fst last)) ++ "->" ++ (show (snd last)) ++ "\n"
getRuleSequence (elem:list) = (show (fst elem)) ++ "->" ++ (show (snd elem)) ++ "\n" ++ (getRuleSequence list)

runMode0 :: String -> IO Int
runMode0 nonParsedCfg = do
	let result = Parser.parseCfg nonParsedCfg
	putStr (getCommaSequence (Parser.getNonterminals result))
	putStr (getCommaSequence (Parser.getTerminals result))
	putStr ((Parser.getStarting result) : "\n" )
	putStr (getRuleSequence (Parser.getRules result))
	return 0

runMode1 :: String -> IO Int
runMode1 nonParsedCfg = do
	let parsedCfg = Parser.parseCfg nonParsedCfg
	let resultRules = Easy.removeEasyRules (Parser.getNonterminals parsedCfg) (Parser.getRules parsedCfg)
	putStr (getCommaSequence (Parser.getNonterminals parsedCfg))
	putStr (getCommaSequence (Parser.getTerminals parsedCfg))
	putStr ((Parser.getStarting parsedCfg) : "\n" )
	putStr (getRuleSequence resultRules)
	return 0

runMode2 :: String -> IO Int
runMode2 nonParsedCfg = do
	return 1 

