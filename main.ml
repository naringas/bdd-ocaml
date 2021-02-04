open Bdd
open Formula

let x = Atom(Var "x")
let x1 = Atom(Var "x1")
let x2 = Atom(Var "x2")
let x3 = Atom(Var "x3")
let y = Atom(Var "y")
let y1 = Atom(Var "y1")
let y2 = Atom(Var "y2")
let y3 = Atom(Var "y3")

let not_x = UniOp(Not, x)
let xAndy = BinOp(x, And, y)

(* f = "((x1 ∨ x2) ∧ (¬x1 ∨ ¬x2))" *)
let f = BinOp(
		BinOp(x1, Or, x2),
		And,
		BinOp(UniOp(Not, x1), Or, UniOp(Not, x2)))

let f_alt = BinOp(
	BinOp(x1, Or, UniOp(Not, x1)),
	And,
	BinOp(x2, Or, UniOp(Not, x2)))

(* f2 = "((x1 ⇔ y1) ∧ (x2 ⇔ y2))" *)
let f2 = BinOp(
	BinOp(x1, BiImplies, Atom(Var "y1")),
	And,
	BinOp(x2, BiImplies, Atom(Var "y2")))

(* f3 = "((x1 ⇔ x2) ∨ x3)" *)
let f3 = BinOp(BinOp(x1, BiImplies, x2), Or, x3)

let f2_order = [|Var "x1"; Var "y1"; Var "x2"; Var "y2"|]
;;
let b3 = expr2bdd f3
(* let t3, ten3 = restrict1 tree false (Var "x2") *)

(*
let show_graphoid b =
	let vars, (nodes, _) = b in
	let root = (Hashtbl.length nodes) - 1 in
	let rec into ?(ts="") k =
		if k < 2 then string_of_int k else
		let {index; high; low} = Hashtbl.find nodes k in
			"["^string_of_int k^" "^atom_to_string vars.(index)^
			"\n"^ts^" lo:("^(into ~ts:(ts^"\t") low)^")"^
			"\n"^ts^" hi:("^(into ~ts:(ts^"\t") high)^")"^
			"\n"^ts^atom_to_string vars.(index)^"]"
	in
	into root
 *)
