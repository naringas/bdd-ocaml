open Bdd
open Formula.Atom
open Formula

let x = Atom(Var "x")
let x1 = Atom(Var "x1")
let x2 = Atom(Var "x2")
let x3 = Atom(Var "x3")

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

let not_x = UniOp(Not, x)
let xAndy = BinOp(x, And, Atom(Var "y"))

let f3 = BinOp(BinOp(x1, BiImplies, x2), And, x3)

;;
let f2_order = [|Var "x1"; Var "y1"; Var "x2"; Var "y2"|]
