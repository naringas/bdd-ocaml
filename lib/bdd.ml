open Formula

type bEntry = {
	index : int; (* index of var in atom array (usually from getVars) *)
	low  : int;
	high : int;
}
let n0 = {index=0; low=0; high=0}
let n1 = {index=1; low=1; high=1}

type bddTables = ((int, bEntry) Hashtbl.t) * ((bEntry, int) Hashtbl.t)
type bdd = (atom array * bddTables)

let bEntry_to_string {index; low; high} =
	"index=" ^ string_of_int index ^ " " ^
	"low=" ^ string_of_int low ^ " " ^
	"high=" ^ string_of_int high

(* funciones "internas" mk, build, bddTables_of_formula *)
let mk (tNodes, tEntries:bddTables) (index:int) ~(low:int) ~(high:int):int =
	if index > 0 && Int.equal low high then low
	else
		let entry = {index; low; high} in
		if Hashtbl.mem tEntries entry then
			Hashtbl.find tEntries entry
		else begin
			let newKey = Hashtbl.length tNodes in
			Hashtbl.add tNodes newKey entry;
			Hashtbl.add tEntries {index; low; high} newKey;
			newKey
		end

let bddTables_of_formula (f:formula) (vars:atom array):bddTables =
	let tNodes = Hashtbl.create ((Array.length vars)+2)
	and tEntries = Hashtbl.create (Array.length vars) in
	Hashtbl.add tNodes 0 n0; Hashtbl.add tNodes 1 n1;
	let rec build (t: formula) (i:int) : int =
		if i > (Array.length vars)-1 then
			if (eval_formula t) then 1 else 0
		else
			let v = vars.(i) in
			let f_inf = to_inf t v in
			mk (tNodes, tEntries) i
				~low:(build f_inf.low (i+1))
				~high:(build f_inf.high (i+1))
	in
	let _ = (build f 0) in
	tNodes, tEntries

let nodes_table bddTables:((int, bEntry) Hashtbl.t) = let nodes, _ = bddTables in nodes

let show_table table =
	for k=(Hashtbl.length table)-1 downto 2 do
		print_int k;
		print_string " : ";
		Hashtbl.find table k |> (fun {index; low; high} -> begin
			"i=" ^ string_of_int index ^ " " ^
			"lo=" ^ string_of_int low ^ " " ^
			"hi=" ^ string_of_int high
		end) |> print_endline;
	done

let show_bdd_table table vars =
	for k=(Hashtbl.length table)-1 downto 2 do
		print_int k;
		print_string " : ";
		Hashtbl.find table k |> (fun {index; low; high} -> begin
			if low < 0 then "" else
			(atom_to_string vars.(index) ^ " " ^
			"lo=" ^ string_of_int low ^ " " ^
			"hi=" ^ string_of_int high)
		end) |> print_endline;
	done;
	print_endline "1 : ⊤";
	print_endline "0 : ⊥"

let show_bdd bdd =
	let vars, (table, _) = bdd in
	print_string "order: "; Array.map (fun a -> atom_to_string a^", ") vars |> Array.iter print_string;
	print_newline();
	show_bdd_table table vars

(* funcion publica publica *)
let expr2bdd ?(vars:atom array option) (f:formula):bdd =
	let vars =
		match vars with
		| None -> getVars f
		| Some vars -> vars
	in
	vars, (bddTables_of_formula f vars)

let show_as_bdd ?(vars:atom array option) (f:formula) =
	print_string "expr: "; to_string f |> print_endline;
	(expr2bdd ?vars f) |> show_bdd

(**
 * RESTRICTION
**)
let varIndex (vars:atom array) (var:atom):int =
	let ret = ref (-1) in vars
	|> Array.iteri (fun index v ->
		if Atom.equals var v then ret := index);
	if !ret = (-1) then raise Not_found else !ret

let restrict1 (tree:bdd) (value:bool) (var:atom) : bdd =
	let vars, (nodes, entries) = tree in
	let newNodes = Hashtbl.copy nodes
	and newEntries = Hashtbl.copy entries in
	(* let newVars = Array.make ((Array.length vars)-1) (Var "") in *)
	let varIndex = varIndex vars var in
	let memoi = Hashtbl.create (Array.length vars) in
	let rec into k : int =
		if Hashtbl.mem memoi k then Hashtbl.find memoi k
		else let result =
			if k < 2 then k else
			let {index; low; high} = Hashtbl.find nodes k in
			if (index < varIndex) then
				mk (newNodes, newEntries) index ~low:(into low) ~high:(into high)
			else if (index = varIndex) then
				if value then (into high) else (into low)
			else (* if (index > varIndex) then *) k
		in Hashtbl.add memoi k result; result
	in
	let _ = into ((Hashtbl.length nodes)-1) in ();
	vars, (newNodes,newEntries)

let show_graphoid (b:bdd) : unit =
	let vars, (nodes, _) = b in
	let root = (Hashtbl.length nodes) - 1 in
	let rec into ?(ts="") k =
		if k < 2 then (print_int k;) else
		let {index; high; low} = Hashtbl.find nodes k in
		begin
			print_newline(); print_string ts;
			print_string ("["^string_of_int k^":"^atom_to_string vars.(index));
			print_newline(); print_string ts;
			(print_string " lo:("); into ~ts:(ts^"\t") low; (print_string ") ");
			print_newline(); print_string ts;
			(print_string " hi:("); into ~ts:(ts^"\t") high; (print_string ") ");
			print_newline(); print_string ts;
			print_string (atom_to_string vars.(index)^"]");
		end
	in into root


(*
let bdd2tabla ((vars, table):bdd) =
	let rows = Int.shift_left 1 (Array.length vars) in
	Array.make_matrix rows 2
	table
 *)
