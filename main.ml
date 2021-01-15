(* open Bdd *)
open Formula

(* f = ((x1 + x2) · (¬x1 + ¬x2)) *)
let f = BinOp(
	BinOp(Atom "x1", Or, Atom "x2"),
	And,
	BinOp(UniOp(Not, Atom "x1"), Or, UniOp(Not, Atom "x2")))

(* let b1 = BNode(Var "x1", Leaf true, Leaf false) *)
(* let b2 = BNode(Var "x2", b1, b1);; *)

(* ;;

let expr2bdd (formula: formula) : bdd =
	let ordering = (getVars formula) in
	let vars = ref
	Leaf true

to_INF (BinOp(Atom (Var "x"), Or, Atom (Var "y")))
 *)


;;

(* let () =
	print_endline (to_string f)
 *)
