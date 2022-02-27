-- Author: Jiri Kristof, <xkrist22@stud.fit.vutbr.cz>
-- Date: February 2022
-- File: Parser.hs

module Parser where


import Data.Char


-- Function for parsing cfg from string into tuple of 4 elements
-- Params:
-- 	cfg: string containing given cfg from stdout or file
-- Returns: (nonterminals, terminals, starting nonterminal, list of rules)
parseCfg :: String -> ([Char], [Char], [Char], [(Char, String)])
parseCfg cfg = ((parseNonterminals nonterminals), (parseTerminals terminals), (parseNonterminals starting), (parseRulesList rulesList))
	where (nonterminals:terminals:starting:rulesList) = splitIntoParts cfg


-- Function splits input cfg string into list by end of line '\n' character
-- Params:
-- 	inputCfg (string): string containing CFG loaded from stdin or file
-- Returns Right: list containing lines of input CFG 
-- Returns Left: string describtion about error
splitIntoParts :: String -> [String]
splitIntoParts inputCfg
	| length (lines inputCfg) == 0 = error "No input CFG given"
	| length (lines inputCfg) == 1 = error "Terminals, starting nonterminal and rules missing"
	| length (lines inputCfg) == 2 = error "Starting nonterminal and rules missing"
	| length (lines inputCfg) == 3 = error "Rules missing"
	| otherwise = lines inputCfg


-- Function parses nonterminal list
-- Params:
-- 	nonterminal list: string in format <LETTER>,<LETTER>,...,<LETTER>, where <LETTER> is character
-- 		from [A-Z]
-- Note:
-- 	nonterminals should be separated only by comma, not spaces
-- 	last comma (after last character) is voluntary, meaning these are same: A,B,C or A,B,C,
-- 	Function is used not only for nonterminal parsing, even for starting nonterminal parsing 
-- Returns: list of nonterminal symbols
parseNonterminals :: String -> [Char] 
parseNonterminals (nonterminal:delimiter:nonterminalList)
	| (isUpper nonterminal) && (delimiter == ',') = (nonterminal : parseNonterminals nonterminalList)
 	| otherwise = error ("Invalid nonterminal (not in [A-Z]): " ++ [nonterminal])
parseNonterminals (nonterminal:[])
	| (isUpper nonterminal) = [nonterminal]
	| otherwise = error ("Invalid nonterminal (not in [A-Z])" ++ [nonterminal])
parseNonterminals [] = []


-- Function parses terminal list
-- Params:
-- 	terminal list: string in format <letter>,<letter>,...,<letter>, where <letter> is character
-- 		from [a-z]
-- Note:
-- 	terminals should be separated only by comma, not spaces
-- 	last comma (after last character) is voluntary, meaning these are same: a,b,c or a,b,c,
-- Returns: list of terminal symbols
parseTerminals :: String -> [Char] 
parseTerminals (terminal:delimiter:terminalList)
	| (isLower terminal) && (delimiter == ',') = (terminal : parseTerminals terminalList)
 	| otherwise = error ("Invalid terminal (not in [a-z]): " ++ [terminal])
parseTerminals (terminal:[])
	| (isLower terminal) = [terminal]
	| otherwise = error ("Invalid terminal (not in [a-z]): " ++ [terminal])
parseTerminals [] = []

parseRulesList :: [String] -> [(Char, String)]
parseRulesList (rule:ruleList) = (parseRule rule) : (parseRulesList ruleList)
parseRulesList [] = []


-- Function parses exactly one rule in format <LETTER>-><RIGHT_SIDE>, where <LETTER> is character from [A-Z] (nonterminal)
-- 	and <RIGHT_SIDE> is substring containing characters from both [A-Z] and [a-z] 
-- Note: 
-- 	right side cannot be empty string (meaning epsilon word)
-- Params:
-- 	rule: string containing rule in given format
-- Returns: tule containing left nonterminal and right side
-- Example:
-- 	> parseRule "A->BAa"  	
-- 	> ('A', "BAa")
parseRule :: String -> (Char, String) 	
parseRule rule@(nonterminal:arrow_dash:arrow_greater:rightRulePart)
	| (isUpper nonterminal) && (arrow_dash == '-') && (arrow_greater == '>') && (checkRightPartOfRule rightRulePart) = (nonterminal, rightRulePart)
	| otherwise = error ("Invalid format of rule: " ++ rule)
parseRule rule = error ("Invalid format of rule: " ++ rule)


-- Function check if right part of rule is valid
-- Params: 
-- 	right part of rule: string containing elements from [a-z] and [A-Z]
-- Returns: True, if string contains only letters and is non empty, else False
checkRightPartOfRule :: String -> Bool
checkRightPartOfRule [] = False
checkRightPartOfRule (letter:[])
	| isLetter letter = True
	| otherwise = False 
checkRightPartOfRule (letter:letterList) 
	| isLetter letter = checkRightPartOfRule letterList
	| otherwise = False


-- Getter for nonterminals
-- Params
-- 	cfg: cfg in internal format representation
-- Returns: nonterminals of cfg
getNonterminals :: ([Char], [Char], [Char], [(String, String)]) -> [Char]
getNonterminals (nonterminals, _, _, _) = nonterminals


-- Getter for nonterminals
-- Params
-- 	cfg: cfg in internal format representation
-- Returns: nonterminals of cfg
getTerminals :: ([Char], [Char], [Char], [(String, String)]) -> [Char]
getTerminals (_, terminals, _, _) = terminals


-- Getter for nonterminals
-- Params
-- 	cfg: cfg in internal format representation
-- Returns: nonterminals of cfg
getStarting :: ([Char], [Char], [Char], [(String, String)]) -> Char
getStarting (_, _, starting, _) = starting !! 0


-- Getter for nonterminals
-- Params
-- 	cfg: cfg in internal format representation
-- Returns: nonterminals of cfg
getRules :: ([Char], [Char], [Char], [(String, String)]) -> [(String, String)]
getRules (_, _, _, rules) = rules

