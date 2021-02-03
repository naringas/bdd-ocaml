open Formula

type bEntry = {
	index : int; (* index of var in atom array (given by getAllVars) *)
	low  : int;
	high : int;
}
let n0 = {index=0; low=0; high=0}
let n1 = {index=1; low=1; high=1}

type bdd = (atom array * (int, bEntry) Hashtbl.t)

let bEntry_to_string {index; low; high} =
	"index=" ^ string_of_int index ^ " " ^
	"low=" ^ string_of_int low ^ " " ^
	"high=" ^ string_of_int high

(* funciones "internas" mk, build, bddTables_of_formula *)
let mk (tNodes) (tEntries) (index:int) ~(low:int) ~(high:int):int =
	if index > 0 && Int.equal low high then low
	else
		let entry = {index; low; high} in
		if Hashtbl.mem tEntries entry then
			Hashtbl.find tEntries entry
		else begin
			let newIndex = Hashtbl.length tNodes in
			Hashtbl.add tNodes newIndex entry;
			Hashtbl.add tEntries {index; low; high} newIndex;
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

let bddTables_of_formula (f:formula) (vars:atom array) =
	let tNodes = Hashtbl.create ((Array.length vars)+2)
	and tEntries = Hashtbl.create (Array.length vars) in
	Hashtbl.add tNodes 0 n0; Hashtbl.add tNodes 1 n1;
	let _ = (build tNodes tEntries vars f 0) in
	tNodes, tEntries

(* funcion casi publica: interfaz para las funciones internas anteriores *)
let formula_to_bddTable (f:formula) (vars:atom array):((int, bEntry) Hashtbl.t) =
	let nodes, _ = bddTables_of_formula f vars in nodes

let show_bdd_table table vars =
	for i=(Hashtbl.length table)-1 downto 2 do
		print_int i;
		print_string " : ";
		Hashtbl.find table i |> (fun {index; low; high} -> begin
			atom_to_string vars.(index) ^ " " ^
			"l=" ^ string_of_int low ^ " " ^
			"h=" ^ string_of_int high
		end) |> print_endline;
	done;
	print_endline "1 : ⊤";
	print_endline "0 : ⊥"

let show_bdd bdd =
	let vars, table = bdd in
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
	vars, formula_to_bddTable f vars

let show_as_bdd ?(vars:atom array option) (f:formula) =
	print_string "expr: "; to_string f |> print_endline;
	(expr2bdd ?vars f) |> show_bdd
