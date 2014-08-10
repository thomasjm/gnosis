
-- Backend API

import qualified Data.Map as Map

import qualified Zora.List as ZList

import Data.Tuple

type Identifier = String

data Rule = Theorem | Definition
type Object = String

type ObjDB = Map.Map Identifier Object
type RuleDB = [Rule]
type DB = (ObjDB, RuleDB)
type Backend = DB

type Op = String
type Value = String
data AST = Empty | OpNode Op [AST] | ValueNode Value

-- inserts object into DB
-- for now, don't allow defining by AST
define_obj :: (Identifier, Object) -> Backend -> Backend
define_obj (identifier, obj) backend@(objdb, ruledb)
	= (Map.insert identifier obj objdb, ruledb)

-- look up rules
-- look up objects

apply_rule :: Rule -> AST -> AST
apply_rule rule ast = ast

get_transformations :: AST -> Backend -> [(AST, Rule)]
get_transformations ast backend@(objdb, ruledb)
	= map swap
	. ZList.map_keep (flip apply_rule $ ast)
	$ applicable_rules
	where
		applicable_rules :: [Rule]
		applicable_rules = filter (can_apply_to ast) ruledb

		can_apply_to :: AST -> Rule -> Bool
		can_apply_to ast rule = True

