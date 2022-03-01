-- Author: Jiri Kristof <xkrist22@stud.fit.vutbr.cz>
-- Date: March 2022
-- File: Easy.hs
 
module Easy where

import qualified Data.Char as Str
import qualified Data.List as List


--TODO
removeEasyRules :: [Char] -> [(Char, String)] -> [(Char, String)]
removeEasyRules [] _ = error "No nonterminals loaded"
removeEasyRules _ [] = []


-- Function generates N_A set for given nonterminal
-- Params:
-- 	list N_i: firstly, it should contain only one nonterminal
-- 	list N_i-1: please, fill this argument with [] empty list (internal use in recursion)
-- 	rules: rules of the CFG
-- Returns: Set N_A = {B| A =>* B} for A in N_0 (starting nonterminal)
-- Note: input rules should be only easy, call getOnlyEasyRules before 
-- 	passing rule list
createNAset nonterminalListI nonterminalListI' easyRuleList
	| nonterminalListI /= nonterminalListI' = createNAset (List.nub (map (!! 0) (map snd [c | c <- easyRuleList, fst c `elem` nonterminalListI]) ++ nonterminalListI)) nonterminalListI easyRuleList
	| otherwise = nonterminalListI


-- method remove non-easy rules from list of rules
-- Params:
-- 	list of rules: list containing all input rules
-- Returns: list, where all rules are in format A->B
getOnlyEasyRules :: [(Char, String)] -> [(Char, String)]
getOnlyEasyRules [] = []
getOnlyEasyRules (rule:ruleList)
	| (length (snd rule) == 1) && (Str.isUpper ((snd rule) !! 0)) = rule : getOnlyEasyRules ruleList
	| otherwise = getOnlyEasyRules ruleList


-- method remove easy rules from list of rules
-- Params:
-- 	list of rules: list containing all input rules
-- Returns: list, where all rules are not easy, meaning not in format A->B
getOnlyNonEasyRules :: [(Char, String)] -> [(Char, String)]
getOnlyNonEasyRules [] = []
getOnlyNonEasyRules (rule:ruleList)
	| (length (snd rule) == 1) && (Str.isUpper ((snd rule) !! 0)) = getOnlyNonEasyRules ruleList
	| otherwise = rule : (getOnlyNonEasyRules ruleList)


