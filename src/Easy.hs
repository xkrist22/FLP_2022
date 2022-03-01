-- Author: Jiri Kristof <xkrist22@stud.fit.vutbr.cz>
-- Date: March 2022
-- File: Easy.hs
 
module Easy where

import qualified Data.Char as str


--TODO
--removeEasyRules :: [Char] -> [(Char, String)] -> [(Char, String)]


--TODO
--createNAset :: Char -> [(Char, String)] -> [Char]


-- method remove non-easy rules from list of rules
-- Params:
-- 	list of rules: list containing all input rules
-- Returns: list, where all rules are in format A->B
getOnlyEasyRules :: [(Char, String)] -> [(Char, String)]
getOnlyEasyRules [] = []
getOnlyEasyRules (rule:ruleList)
	| (length (snd rule) == 1) && (str.isUpper ((snd rule) !! 0)) = rule : getOnlyEasyRules ruleList
	| otherwise = getOnlyEasyRules ruleList


-- Method returns all rules for given nonterminal
-- Params:
-- 	nonterminal: char specifying nonterminal, for which fonction
-- 		returns all rules
-- 	list of rules: list containing rules, from which are selected rules
-- Return: list of rules, where first element is equal to selected nonterminal	
getRulesForNonterminal :: Char -> [(Char, String)] -> [(Char, String)]
getRulesForNonterminal _ [] = []
getRulesForNonterminal nonterminal (rule:ruleList)
	| fst rule == nonterminal = rule : getRulesForNonterminal ruleList
	| otherwise = getRulesForNonterminal ruleList

