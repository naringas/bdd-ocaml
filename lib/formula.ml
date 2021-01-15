module Atoms = Set.Make(String)

type uniOp = Not
type binOp = And | Or | Implies
	(* | BiImplies | Xor | Nor | Nand *)

type formula =
	| Atom of string
	| UniOp of uniOp * formula
	| BinOp of formula * binOp * formula

let getVars (f : formula) : Atoms.t =
	let rec getAllVars = function
		| Atom f -> f :: []
		| UniOp (_op, f) -> getAllVars f
		| BinOp (f1, _op, f2) -> (getAllVars f1) @ (getAllVars f2)
	in
	Atoms.of_list (getAllVars f)

let rec to_string (f : formula) : string =
	let binNode_to_string f1 op f2 =
		match op with
		| And ->
			String.concat ""
			("(" :: to_string f1 ::" · ":: to_string f2 :: ")" :: [])
		| Or  ->
			String.concat ""
			("(" :: to_string f1 ::" + ":: to_string f2 :: ")" :: [])
		| Implies ->
			String.concat ""
			("(" :: to_string f1 ::" -> ":: to_string f2 :: ")" :: [])
	in
	match f with
	| Atom f -> f
	| UniOp (_op, f) -> "¬" ^ to_string f
	| BinOp (f1, op, f2) -> binNode_to_string f1 op f2
