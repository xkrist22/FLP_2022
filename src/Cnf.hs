module Cnf where

import Data.Char
import Data.List

type TRuleIn = (Char, String)
type TRuleOut = (String, String) 


getCnfRules :: [TRuleIn] -> [TRuleOut]
getCnfRules rules = nub $ alreadyOkRules ++ easedRules ++ terminalRules
	where
		alreadyOkRules = fromInToOutFormat (takeOkRules rules)
		easedRules = (regenerateRuleList (takeNotOkRules rules))
		terminalRules = addTerminalRules easedRules


-- Method takes all nonterminals from rules and return string in output format of program (comma-separated nonterminals)
getNewNonterminals :: [TRuleOut] -> String
getNewNonterminals [] = ""
getNewNonterminals (lastRule:[]) = fst lastRule
getNewNonterminals (rule:ruleList) =  fst rule ++ "," ++ getNewNonterminals ruleList


-- Method reformat input list of rules in format [(Char, String)] into format [(String, String)]
fromInToOutFormat :: [TRuleIn] -> [TRuleOut]
fromInToOutFormat [] = []
fromInToOutFormat (rule:ruleList) = [([fst rule], snd rule)] ++ fromInToOutFormat ruleList


-- Function takes rules from given list of rules, which are in format
-- A->a or A->BC
-- Params:
-- 	list of rules: input list of rules
-- Returns: list of rules already in CNF
takeOkRules :: [TRuleIn] -> [TRuleIn]
takeOkRules [] = []
takeOkRules (rule:ruleList)
	| ruleOk1 rule || ruleOk2 rule = rule : (takeOkRules ruleList)
	| otherwise = takeOkRules ruleList
	where
		ruleOk1 testedRule = (length (snd testedRule) == 1) && (isLower $ (snd testedRule) !! 0)
		ruleOk2 testedRule = (length (snd testedRule) == 2) && (isUpper $ (snd testedRule) !! 0) && (isUpper $ (snd testedRule) !! 1)


-- Function takes rules from given list of rules, which are not in format 
-- A->a or A->BC
-- Params:
-- 	list of rules: input list of rules
-- Returns: list of rules not in CNF
takeNotOkRules :: [TRuleIn] -> [TRuleIn]
takeNotOkRules ruleList = (ruleList \\ (takeOkRules ruleList))


-- This algorithm takes rules, where length of right part is bigger than 2 and create new rules in CNF 
-- Args:
--	list of rules: list of input rules in format [(Char, String)]
-- Returns: list of output rules in format [(String, String)]
-- Example:
-- regenerateRuleList [('A', "ABc")]
-- [("A", "A'<Bc>"), ("<Bc>", "B'c'")]  
-- Notes: 
-- 	- output format of rules are different, left right 
-- 		is String, not Char
-- 	- function should be called only with non-easy rules 
-- 		(filter using takeNotOkRules), otherwise there 
-- 		is risk of exception  
regenerateRuleList :: [TRuleIn] -> [TRuleOut]
regenerateRuleList [] = []
regenerateRuleList (rule:ruleList) = (generateNewRules [fst rule] (snd rule)) ++ (regenerateRuleList ruleList)


-- Method generates new rules for one rule
-- Args:
-- 	rule in format (String, String)
generateNewRules :: String -> String -> [TRuleOut]
generateNewRules left origRight@(firstElem:others)
	| isLongerThan2 && isTerminal = [(left, right1)] ++ (generateNewRules untouchedChain others)
	| isLongerThan2 && isNonterminal = [(left, right2)] ++ (generateNewRules untouchedChain others)
	| isLongExact2 && bothNonterminal = [(left, endingChain1)]
	| isLongExact2 && bothTerminal = [(left, endingChain2)] 
	| isLongExact2 && fstNonterminal = [(left, endingChain3)]
	| isLongExact2 = [(left, endingChain4)]
	| otherwise = error "Cannot generate new rules in CNF from given rule"
	where
		isLongerThan2 = (length origRight > 2)
		isLongExact2 = (length origRight == 2)
		right1 = [firstElem] ++ "'<" ++ others ++ ">"
		right2 = [firstElem] ++ "<" ++ others ++ ">"
		untouchedChain = "<" ++ others ++ ">"
		endingChain1 = [firstElem] ++ others
		endingChain2 = [firstElem] ++ "'" ++ others ++ "'"
		endingChain3 = [firstElem] ++ others ++ "'"
		endingChain4 = [firstElem] ++ "'" ++ others
		isNonterminal = isUpper firstElem
		isTerminal = isLower firstElem
		bothNonterminal = (isUpper firstElem) && (isUpper (others !! 0))
		bothTerminal = (isLower firstElem) && (isLower (others !! 0))
		fstNonterminal = isUpper firstElem
generateNewRules _ _ = error "Internal error during CFG generating"

-- Method add new rules for terminal symbols
-- Params:
-- 	list of rules: list of rules in format [(String, String)]
-- Returns: list of rules in format a'->a in internal TRuleOut format
-- Example:
-- 	addTerminalRules ["X", "A'b'C'd'"]
--	> [("b'", "b"), ("d'", "d")]
addTerminalRules :: [TRuleOut] -> [TRuleOut]
addTerminalRules [] = []
addTerminalRules (rule:ruleList) = (addTerminalRulesOne (addTerminalParseRight (snd rule))) ++ addTerminalRules ruleList


-- method creates new terminal rules, takes list of terminals and 
-- Params:
-- 	list of terminals: list of terminals
-- Returns: list of rules in format a'->a 
-- Example:
-- 	addTerminalRulesOne "ab"
-- 	> [("a'", "a"), ("b'", "b")]
addTerminalRulesOne :: [Char] -> [TRuleOut]
addTerminalRulesOne [] = []
addTerminalRulesOne (terminal:terminals) = [([terminal] ++ "'", [terminal]) ] ++ addTerminalRulesOne terminals


-- Generates list of nonterminals created from terminal
-- Args:
-- 	right part of rule
-- Returns: list if terminals, from which nonterminals were created
-- Note: should be called after adding new rules in CNF
-- Example:
-- 	addTerminalParseRight "Ab'CDe'"
-- 	> "be"
addTerminalParseRight :: String -> String
addTerminalParseRight (char1:char2:other)
	| isNonterminalFromTerminal = [char1] ++ addTerminalParseRight other
	| otherwise = addTerminalParseRight (char2:other)
	where
		isNonterminalFromTerminal = (isLower char1) && (char2 == '\'')
addTerminalParseRight _ = ""

