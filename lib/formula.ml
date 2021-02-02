module Atom = struct
	type atom =
		| Var of string
		| B of bool
	type t = atom
	let compare a b =
		match a, b with
		| (Var a, Var b) -> String.compare a b
		| (Var _a, B _b) -> 1
		| (B _a, Var _b) -> -1
		| (B a, B b) -> Bool.compare a b
	let equals a b = Int.equal 0 (compare a b)
	let to_string = function
		| Var a -> a
		| B a -> if a then "⊤" else "⊥"
end
module ASet = Set.Make(Atom)

(* shortcuts *)
type atom = Atom.t
let atom_to_string = Atom.to_string

type uniOp = Not
type binOp = And | Or | Implies | BiImplies (* | Xor | Nor | Nand *)
type formula =
	| Atom of atom
	| UniOp of uniOp * formula
	| BinOp of formula * binOp * formula

(* If-then-else normal form *)
type inf = {
	low: formula;
	high: formula
}

let getVars (f:formula):Atom.t array =
	let rec getAllVars = function
		| Atom f -> f :: []
		| UniOp (_op, f) -> getAllVars f
		| BinOp (f1, _op, f2) -> (getAllVars f1) @ (getAllVars f2)
	in
	(* :( there must be a better way, using ASet is too expensive *)
	getAllVars f |> ASet.of_list |> ASet.to_seq |> Array.of_seq

let rec to_string (f:formula):string =
	let string_from_op = begin function
		| And -> " ∧ "
		| Or  -> " ∨ "
		| Implies -> " ⇒ "
		| BiImplies -> " ⇔ "
	end
	in match f with
	| Atom f -> atom_to_string f
	| UniOp (Not, f) -> "¬" ^ to_string f
	| BinOp (f1, op, f2) -> "("^(to_string f1) ^ (string_from_op op) ^ (to_string f2)^")"

let rec sub_bool (f:formula) (value:bool) (x:atom):formula =
	match f with
	| Atom a -> begin
		match a with
		| B b -> Atom (B b)
		| Var name ->
			if String.equal name (atom_to_string x)
			then Atom (B value)
			else Atom (Var name)
	end
	| UniOp (Not, f) -> UniOp(Not, sub_bool f (value) x)
	| BinOp (f1, op, f2) ->
		BinOp((sub_bool f1 value x), op, (sub_bool f2 value x))

(* do a f[value/variable] substitution *)
let rec substitute (f:formula) (value:atom) (x:atom):formula =
	match value with
	| B value -> sub_bool f value x
	| Var value -> (
		match f with
		| Atom a -> begin
			match a with
			| B b -> Atom (B b)
			| Var name -> (
				if String.equal name (atom_to_string x)
				then Atom (Var value)
				else Atom (Var name))
		end
		| UniOp (Not, f) -> UniOp(Not, (substitute f (Var value) x))
		| BinOp (f1, op, f2) ->
			BinOp((substitute f1 (Var value) x), op, (substitute f2 (Var value) x)))

let to_inf (f:formula) (var:atom):inf = {
	low=(substitute f (B false) var);
	high=(substitute f (B true) var)}

let rec inf_desc (f:formula) (var:atom) : inf =
	match var with
	| B _ -> assert false
	| Var v -> begin
		match f with
		| Atom t ->
			if String.equal (atom_to_string t) v
			then to_inf f var else {low=f; high=f}
		(* voltea las variables para aplicar el not *)
		| UniOp (Not, t) ->
			let t_inf = inf_desc t var
			in {low=t_inf.high; high=t_inf.low}
		| BinOp (p, op, q) -> {
			low=BinOp(
				(substitute p (B false) var),
				op,
				(substitute q (B false) var));
			high=BinOp(
				(substitute p (B true) var),
				op,
				(substitute q (B true) var))}
	end

(* evaluates a formula in which all atoms are boolean (NO VARIABLES) *)
let rec eval_formula form : bool =
	match form with
	| Atom f -> begin
		match f with
		| Var _ -> assert false
		| B b -> b
	end
	| UniOp (Not, f) -> not (eval_formula f)
	| BinOp (p, op, q) -> begin
		match op with
		| Or  -> ((eval_formula p) || (eval_formula q))
		| And -> ((eval_formula p) && (eval_formula q))
		| Implies -> (not (eval_formula p) || (eval_formula q))
		| BiImplies -> begin
			let evP = eval_formula p
			and evQ = eval_formula q in
			(evP && evQ) || (not evP && not evQ)
		end
	end

(* funciones de prueba/debug *)
let show_inf_of_f f =
	let to_inf_all (f:formula) (vars:atom array) =
		Array.map (fun var ->
			Atom var, (substitute f (B true) var), (substitute f (B false) var)) vars
	and inf_to_string (a,b,c) =
		(to_string a)^"->"^(to_string b)^", "^(to_string c)
	in to_inf_all f (getVars f) |> (Array.map inf_to_string)

let print_f_inf f = show_inf_of_f f |> Array.iter print_endline
