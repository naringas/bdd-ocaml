type uniOp = Not
type binOp = And | Or | Implies | BiImplies
	(* | Xor | Nor | Nand *)

module Atom = struct
	type t =
		| Var of string
		| B of bool

	let compare a b =
		match a, b with
		| (Var a, Var b) -> String.compare a b
		| (Var _a, B _b) -> 1
		| (B _a, Var _b) -> -1
		| (B a, B b) -> Bool.compare a b
end
module ASet = Set.Make(Atom)

(* open Atom *)
type atom = Atom.t  (* shortcut *)

type formula =
	| Atom of atom
	| UniOp of uniOp * formula
	| BinOp of formula * binOp * formula

let atom_to_string = let open Atom in function
	| Var a -> a
	| B a -> if a then "⊤" else "⊥"


let getVars (f : formula): Atom.t array =
	let rec getAllVars = function
		| Atom f -> f :: []
		| UniOp (_op, f) -> getAllVars f
		| BinOp (f1, _op, f2) -> (getAllVars f1) @ (getAllVars f2)
	in
	(* :( there must be a better way, using ASet is too expensive *)
	getAllVars f |> ASet.of_list |> ASet.to_seq |> Array.of_seq

let rec to_string (f : formula) : string =
	let binNode_to_string f1 op f2 =
		match op with
		| And ->
			String.concat ""
			("(" :: to_string f1 ::" ∧ ":: to_string f2 :: ")" :: [])
		| Or  ->
			String.concat ""
			("(" :: to_string f1 ::" ∨ ":: to_string f2 :: ")" :: [])
		| Implies ->
			String.concat ""
			("(" :: to_string f1 ::" ⇒ ":: to_string f2 :: ")" :: [])
		| BiImplies ->
			String.concat ""
			("(" :: to_string f1 ::" ⇔ ":: to_string f2 :: ")" :: [])
	in
	match f with
	| Atom f -> atom_to_string f
	| UniOp (_op, f) -> "¬" ^ to_string f
	| BinOp (f1, op, f2) -> binNode_to_string f1 op f2

(* do a f[value/variable] substitution *)
let rec substitute (f: formula) (value: bool) (x: atom) : formula =
	match f with
	| Atom a -> (
		match a with
		| B _ -> f
		| Var name -> (
			if String.equal name (atom_to_string x)
			then Atom (B value)
			else Atom a))
	| UniOp (op, f) ->
		(* todo if op is get, flip the bool if subbing this atom *)
		UniOp(op, (substitute f value x))
	| BinOp (f1, op, f2) ->
		BinOp((substitute f1 value x), op, (substitute f2 value x))

let to_INF (f: formula) (i: int) (nodes: atom array): (atom * formula * formula) =
	let n = nodes.(i) in
	n, (substitute f true n), (substitute f false n)

(*
let to_INF (f: formula) (x: x) (formula * formula) =
	match x with
	| B x ->
	| Var x ->
*)
;;
