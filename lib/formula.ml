module Atoms = Set.Make(String)

type uniOp = Not
type binOp = And | Or | Implies | BiImplies
	(* | Xor | Nor | Nand *)

type atom =
	| Var of string
	| B of bool

type formula =
	| Atom of atom
	| UniOp of uniOp * formula
	| BinOp of formula * binOp * formula

let atom_to_string = function
	| Var a -> a
	| B a -> if a then "1" else "0"

let getVars (f : formula) : Atoms.t =
	let rec getAllVars = function
		| Atom f -> (atom_to_string f) :: []
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
		| BiImplies ->
			String.concat ""
			("(" :: to_string f1 ::" <-> ":: to_string f2 :: ")" :: [])
	in
	match f with
	| Atom f -> atom_to_string f
	| UniOp (_op, f) -> "¬" ^ to_string f
	| BinOp (f1, op, f2) -> binNode_to_string f1 op f2

;;
