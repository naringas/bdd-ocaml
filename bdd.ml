open Formula

type bdd =
	| Leaf of bool
	| BNode of formula * bdd * bdd

let left (b : bdd) : bdd =
	match b with
	| BNode (_label, left, _right) -> left
	| Leaf l -> Leaf l

let right (b : bdd) : bdd =
	match b with
	| BNode (_label, _left, right) -> right
	| Leaf l -> Leaf l
