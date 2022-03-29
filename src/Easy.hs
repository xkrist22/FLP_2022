-- Author: Jiri Kristof <xkrist22@stud.fit.vutbr.cz>
-- Date: March 2022
-- File: Easy.hs
 
module Easy where

import qualified Data.Char as Str
import qualified Data.List as List


-- Method takes list of nonterminals and rules and removes easy rules
-- Params:
-- 	nonterminals: list of chars containing nonterminals of CFG
-- 	rule list: list containing rules in format (left nonterminal, right side)
-- Returns: 
removeEasyRules :: [Char] -> [(Char, String)] -> [(Char, String)]
removeEasyRules [] _ = []
removeEasyRules _ [] = []
removeEasyRules (nonterminal:nonterminalList) ruleList = nonEasyRulesForNonterminal ++ nonEasyRulesForOther
	where
		nonEasyRulesForNonterminal = removeEasyRulesForNonterminal nonterminal ruleList 
		nonEasyRulesForOther = removeEasyRules nonterminalList ruleList


-- Function removes easy rules for one nonterminal
-- Params:
-- 	nonterminal: selected nonterminal, for which easy rules are removed
-- 	rule list: list of all rules from input
-- Returns: list of non easy rules, where on left side is selected nonterminal
removeEasyRulesForNonterminal :: Char -> [(Char, String)] -> [(Char, String)]
removeEasyRulesForNonterminal nonterminal ruleList = newRulesForNonterminal
	where
		-- create N_A list for selected nonterminal A
		nAList = createNAset [nonterminal] [] (getOnlyEasyRules ruleList)
		-- take rules, where on left side is any nonterminal from N_A list
		aRules = [rule | rule <- ruleList, fst rule `elem` nAList]
		-- take only right sides of rules
		rightSidesOfNonEasyRules = map snd $ getOnlyNonEasyRules aRules
		-- create new rules for given nonterminal and filtered right sides of rules
		newRulesForNonterminal = zip (repeat nonterminal) rightSidesOfNonEasyRules


-- Function generates N_A set for given nonterminal
-- Params:
-- 	list N_i: firstly, it should contain only one nonterminal
-- 	list N_(i-1): please, fill this argument with [] empty list (internal use in recursion)
-- 	rules: rules of the CFG
-- Returns: Set N_A = {B| A =>* B} for A in N_0 (starting nonterminal)
-- Note: input rules should be only easy, call getOnlyEasyRules before 
-- 	passing rule list
createNAset :: [Char] -> [Char] -> [(Char, String)] -> [Char]
createNAset nonterminalListI nonterminalListI' easyRuleList
	| setDiffersFromPreviousStep = createNAset nIListWithoutDuplicates nonterminalListI easyRuleList
	| otherwise = nonterminalListI
	where
		-- condition N_i != N_(i-1)
		setDiffersFromPreviousStep = nonterminalListI /= nonterminalListI' 
		-- list od rules, where nonterminal on left side is on N_(i-1)
		ruleList = [c | c <- easyRuleList, fst c `elem` nonterminalListI]
		-- take only right parts of rules (create N_i) and concat it with already created N_(i-1)
		nIList = map (!!0) (map snd ruleList) ++ nonterminalListI
		-- remove duplicates
		nIListWithoutDuplicates = List.nub nIList


-- method remove non-easy rules from list of rules
-- Params:
-- 	list of rules: list containing all input rules
-- Returns: list, where all rules are in format A->B
getOnlyEasyRules :: [(Char, String)] -> [(Char, String)]
getOnlyEasyRules [] = []
getOnlyEasyRules (rule:ruleList)
	| oneNonterminalOnLeftSide = rule : getOnlyEasyRules ruleList
	| otherwise = getOnlyEasyRules ruleList
	where
		oneNonterminalOnLeftSide = (length (snd rule) == 1) && (Str.isUpper ((snd rule) !! 0)) 


-- method remove easy rules from list of rules
-- Params:
-- 	list of rules: list containing all input rules
-- Returns: list, where all rules are not easy, meaning not in format A->B
getOnlyNonEasyRules :: [(Char, String)] -> [(Char, String)]
getOnlyNonEasyRules [] = []
getOnlyNonEasyRules (rule:ruleList)
	| oneNonterminalOnLeftSide = getOnlyNonEasyRules ruleList
	| otherwise = rule : (getOnlyNonEasyRules ruleList)
	where
		oneNonterminalOnLeftSide = (length (snd rule) == 1) && (Str.isUpper ((snd rule) !! 0)) 

