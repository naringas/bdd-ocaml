open Bdd
open Formula

let f = BinOp(
	BinOp(Atom "x1", Or, Atom "x2"),
	And,
	BinOp(UniOp(Not, Atom "x1"), Or, UniOp(Not, Atom "x2")))

let b1 = BNode(Atom "x1", Leaf true, Leaf false)
let b2 = BNode(Atom "x2", b1, b1);;

(* ;;

let expr2bdd (formula: formula) : bdd =
	let ordering = (getVars formula) in
	let vars = ref
	Leaf true

 *)

;;

let () =
	print_endline (to_string f)

