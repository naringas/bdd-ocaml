type uniOp = Not

type binOp = And | Or
	(* | Implies | BiImplies | Xor | Nor | Nand *)

type formula =
	| Atom of string
	| UniNode of uniOp * formula
	| BinNode of formula * binOp * formula

module Atoms = Set.Make(String)

let getVars (f : formula) : Atoms.t =
	let rec getAllVars f =
		match f with
		| Atom f -> f :: []
		| UniNode (_op, f) -> getAllVars f
		| BinNode (f1, _op, f2) -> (getAllVars f1) @ (getAllVars f2)
	in
	Atoms.of_list (getAllVars f)

;;
let f = BinNode(
	BinNode(Atom "x1", Or, Atom "x2"),
	And,
	BinNode(UniNode(Not, Atom "x1"), Or, UniNode(Not, Atom "x2")))
