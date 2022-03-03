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
removeEasyRules (nonterminal:nonterminalList) ruleList = (removeEasyRulesForNonterminal nonterminal ruleList) ++ (removeEasyRules nonterminalList ruleList)

-- Function removes easy rules for one nonterminal
-- Params:
-- 	nonterminal: selected nonterminal, for which easy rules are removed
-- 	rule list: list of all rules from input
-- Returns: list of non easy rules, where on left side is selected nonterminal
removeEasyRulesForNonterminal :: Char -> [(Char, String)] -> [(Char, String)]
-- take rules, where on left side is nonterminal from N_A list of selected nonterminal A, then take only non easy rules from 
removeEasyRulesForNonterminal nonterminal ruleList = zip (repeat nonterminal) (map snd (getOnlyNonEasyRules [rule | rule <- ruleList, fst rule `elem` nAList]))
	where nAList = createNAset [nonterminal] [] (getOnlyEasyRules ruleList)

-- Function generates N_A set for given nonterminal
-- Params:
-- 	list N_i: firstly, it should contain only one nonterminal
-- 	list N_i-1: please, fill this argument with [] empty list (internal use in recursion)
-- 	rules: rules of the CFG
-- Returns: Set N_A = {B| A =>* B} for A in N_0 (starting nonterminal)
-- Note: input rules should be only easy, call getOnlyEasyRules before 
-- 	passing rule list
createNAset nonterminalListI nonterminalListI' easyRuleList
	-- create list of rules, where nonterminal on left side is in N_i-1 (actuall is N_i), then create list from right parts of selected rules
	-- then concat created list N_i with N_i-1 and removes duplicates and call createNAlist to simulate next iteration
	| nonterminalListI /= nonterminalListI' = createNAset (List.nub (map (!! 0) (map snd [c | c <- easyRuleList, fst c `elem` nonterminalListI]) ++ nonterminalListI)) nonterminalListI easyRuleList
	-- in case of no change, return created list
	| otherwise = nonterminalListI


-- method remove non-easy rules from list of rules
-- Params:
-- 	list of rules: list containing all input rules
-- Returns: list, where all rules are in format A->B
getOnlyEasyRules :: [(Char, String)] -> [(Char, String)]
getOnlyEasyRules [] = []
getOnlyEasyRules (rule:ruleList)
	-- if on left side is only one Nonterminal, then add it to return list
	| (length (snd rule) == 1) && (Str.isUpper ((snd rule) !! 0)) = rule : getOnlyEasyRules ruleList
	-- else do not add actuall rule to returned rules
	| otherwise = getOnlyEasyRules ruleList


-- method remove easy rules from list of rules
-- Params:
-- 	list of rules: list containing all input rules
-- Returns: list, where all rules are not easy, meaning not in format A->B
getOnlyNonEasyRules :: [(Char, String)] -> [(Char, String)]
getOnlyNonEasyRules [] = []
getOnlyNonEasyRules (rule:ruleList)
	-- if on right side is not only one nonterminal, append rule to return list
	| (length (snd rule) == 1) && (Str.isUpper ((snd rule) !! 0)) = getOnlyNonEasyRules ruleList
	-- else do not append rule
	| otherwise = rule : (getOnlyNonEasyRules ruleList)


