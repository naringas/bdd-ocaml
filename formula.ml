module Atoms = Set.Make(String)

type uniOp = Not
type binOp = And | Or
	(* | Implies | BiImplies | Xor | Nor | Nand *)
type formula =
	| Atom of string
	| UniNode of uniOp * formula
	| BinNode of formula * binOp * formula

let getVars (f : formula) : Atoms.t =
	let rec getAllVars = function
		| Atom f -> f :: []
		| UniNode (_op, f) -> getAllVars f
		| BinNode (f1, _op, f2) -> (getAllVars f1) @ (getAllVars f2)
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
	in
	match f with
	| Atom f -> f
	| UniNode (_op, f) -> "¬" ^ to_string f
	| BinNode (f1, op, f2) -> binNode_to_string f1 op f2
