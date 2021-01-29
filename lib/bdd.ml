open Formula

type bdd =
	| Leaf of bool
	| Node of atom * bdd * bdd (* := Var, Low, High *)

let left (b : bdd) : bdd =
	match b with
	| Node (_label, left, _right) -> left
	| Leaf l -> Leaf l

let right (b : bdd) : bdd =
	match b with
	| Node (_label, _left, right) -> right
	| Leaf l -> Leaf l

type bEntry = {
	index : int;
	low  : int;
	high : int;
}
let n0 = {index=0; low=0; high=0}
let n1 = {index=1; low=1; high=1}

let bEntry_to_string {index; low; high} =
	"index=" ^ string_of_int index ^ " " ^
	"low=" ^ string_of_int low ^ " " ^
	"high=" ^ string_of_int high

let mk (tNodes) (tEntries) (index:int) ~(low:int) ~(high:int):int =
	if Int.equal low high
	then low
	else let entry = {index; low; high} in
	(* entry |> bEntry_to_string |> print_endline; *)
	if Hashtbl.mem tEntries entry then
		Hashtbl.find tEntries entry
	else begin
		let newIndex = Hashtbl.length tEntries in
		Hashtbl.add tEntries entry newIndex;
		Hashtbl.add tNodes newIndex entry;
		("inserted " ^ string_of_int newIndex) |> print_endline;
		newIndex
	end

let rec build tNodes tEntries vars (t: formula) (i:int) : int =
	if i > (Array.length vars)-1 then
		if (eval_formula t) then 1 else 0
	else
		let v = vars.(i) in
		let f_inf = inf_desc t v in
		mk tNodes tEntries i
			~low:(build tNodes tEntries vars f_inf.low (i+1))
			~high:(build tNodes tEntries vars f_inf.high (i+1))

let bdd_of_formula (f: formula) =
	let vars = getVars f in
	let tNodes = Hashtbl.create ((Array.length vars)+2)
	and tEntries = Hashtbl.create (Array.length vars) in
	Hashtbl.add tNodes 0 n0; Hashtbl.add tNodes 1 n1;
	Hashtbl.add tEntries n0 0; Hashtbl.add tEntries n1 1;
	let _ = (build tNodes tEntries vars f 0) in
	tNodes, tEntries

let expr2bdd = bdd_of_formula

(* testing/debug function *)
let bdd_and_show f =
	let tNodes, tEntries = bdd_of_formula f in
	Hashtbl.iter (fun key v -> begin
		print_int key;
		print_string " : ";
		v |> bEntry_to_string |> print_endline
	end) tNodes;
	Hashtbl.iter (fun key v -> begin
		key |> bEntry_to_string |> print_string;
		print_string " : ";
		print_int v;
		print_newline();
	end) tEntries;
