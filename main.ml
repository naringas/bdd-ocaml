open Bdd
open Formula

let f = BinNode(
	BinNode(Atom "x1", Or, Atom "x2"),
	And,
	BinNode(UniNode(Not, Atom "x1"), Or, UniNode(Not, Atom "x2")))

let b1 = Node(Atom "x1", Leaf true, Leaf false)
let b2 = Node(Atom "x2", b1, b1);;

;;

let () =
	print_endline (to_string f)

