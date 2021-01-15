open Formula

type bdd =
	| Leaf of bool
	| Node of string * bdd * bdd

let left (b : bdd) : bdd =
	match b with
	| Node (_label, left, _right) -> left
	| Leaf l -> Leaf l

let right (b : bdd) : bdd =
	match b with
	| Node (_label, _left, right) -> right
	| Leaf l -> Leaf l
