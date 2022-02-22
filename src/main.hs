-- Author: Jiri Kristof <xkrist22@stud.fit.vutbr.cz>
-- Date: February 2022
-- File: main.hs
--

import qualified System.Environment as Env
import qualified Parse
import qualified Easy
import qualified Cfg

-- Main function
main = do
	args <- Env.getArgs


-- Function parse arguments from cmd
-- Params:
-- 	args: list containing arguments given from terminal
-- Returns: tuple, where first element is mod of program
-- 	second element contains file where CFG is stored (or stdout) 
-- Note: if arguments are invalid, then second element of return
-- 	tuple contain error describtion
parseArgs args
	-- no args given
	| (length args == 0) = (2, "stdout")
	-- arg -h given
	| (length args == 1 && "-h" `elem` args) = (3, "HELP") 
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
	| otherwise = (3, "INPUT PARAMETERS ERROR")


-- Function reads input CFG from stdout
-- Returns: string containing input CFG
readCfgFromStdout = "TODO" 
 

-- Function reads input CFG from given filename
-- Params:
-- 	filename: string containing name of the file where CFG is stored
-- Returns: string containing input CFG	
readCfgFromFile filename = "TODO"

-- Function for printing help to stdout
printHelp = do
	putStrLn "TODO"


-- POZNAMKA PRO ME: DOPLN TY ZASRATE MODULY 
