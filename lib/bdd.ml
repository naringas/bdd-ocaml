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


type bddEntry = {
	index : int;
	low  : int option;
	high : int option;
}

let tT = Hashtbl.create ~random:false 10
let n0 = {index=0; low=None; high=None};;
let n1 = {index=1; low=None; high=None};;
Hashtbl.add tT "0" n0;
Hashtbl.add tT "1" n1
let tH = Hashtbl.create ~random:false 10

let mk ({index; low; high}: bddEntry) =
	match (low, high) with
	| (None, None)
	| (Some _, None)
	| (None, Some _) -> assert false
	| (Some low, Some high) -> (
		let entry = {index; low=Some low; high=Some high} in
		if low == high
		then (string_of_int low)
		else if Hashtbl.mem tH entry
		then (Hashtbl.find tH entry)
		else (
			Hashtbl.add tH entry (string_of_int index);
			Hashtbl.add tT (string_of_int index) entry;
			string_of_int index))
(*
let build t =
	let rec buildIn t i =
 *)
