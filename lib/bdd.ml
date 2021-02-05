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
	if Int.equal low high (* !!! esto evita bdd de un solo nodo pero rompe la reduccion en apply && index > 0 *)
	then low
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

(**
 * CREAR BDD expr2bdd
**)
let expr2bdd ?(vars:atom array option) (f:formula):bdd =
	let vars =
		match vars with
		| None -> getVars f
		| Some vars -> vars
	in
	vars, (bddTables_of_formula f vars)

let show_as_bdd ?(vars:atom array option) (f:formula) =
	print_string "expr: "; to_string f |> print_endline;
	let b = (expr2bdd ?vars f) in b |> show_bdd; b

let show_graphoid (b:bdd) : unit =
	let vars, (nodes, _) = b in
	let root = (Hashtbl.length nodes) - 1 in
	let rec into ?(ts="") k =
		if k < 2 then (print_int k;) else
		let {index; high; low} = Hashtbl.find nodes k in
		begin
			print_newline(); print_string ts;
			print_string ("["^string_of_int k^" "^atom_to_string vars.(index));
			print_newline(); print_string ts;
			(print_string " lo:("); into ~ts:(ts^"\t") low; (print_string ") ");
			print_newline(); print_string ts;
			(print_string " hi:("); into ~ts:(ts^"\t") high; (print_string ") ");
			print_newline(); print_string ts;
			print_string (atom_to_string vars.(index)^"]");
		end
	in into root


(**
 * RESTRICTION
**)
let varIndex (vars:atom array) (var:atom):int =
	let ret = ref (-1) in
	vars |> Array.iteri (fun index v ->
		if Atom.equal var v then ret := index);
	if !ret = (-1) then raise Not_found else !ret

let restrict1 (value:bool) (var:atom) (tree:bdd) : bdd =
	let vars, (nodes, entries) = tree in
	let newNodes = Hashtbl.copy nodes
	and newEntries = Hashtbl.copy entries in
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


(**
 * APPLY
**)
let mergeVars vars1 vars2 : atom array =
	Array.to_list vars1 @ Array.to_list vars2 |> ASet.of_list |> ASet.to_seq |> Array.of_seq

let atom_of_int i = if i = 0 then B false else B true

let apply (op:binOp) (b1:bdd) (b2:bdd) =
	let vars1, (nodes1, _entries1) = b1
	and vars2, (nodes2, _entries2) = b2 in
	let newVars = (mergeVars vars1 vars2) in
	let newNodes = Hashtbl.create ((Array.length newVars)+2)
 	and newEntries = Hashtbl.create (Array.length newVars) in
	Hashtbl.add newNodes 0 n0; Hashtbl.add newNodes 1 n1;

	let memoi = Hashtbl.create ((Array.length newVars)*2) in
	let rec app t1 t2 =
		if Hashtbl.mem memoi (t1,t2) then Hashtbl.find memoi (t1,t2)
		else let result =
			if t1 < 2 && t2 < 2 then
				eval_formula (BinOp(Atom (atom_of_int t1), op, Atom (atom_of_int t2))) |> Bool.to_int
			else
				let e1 = Hashtbl.find nodes1 t1
				and e2 = Hashtbl.find nodes2 t2 in
				(* print_int t1; bEntry_to_string e1 |> print_endline;
				print_int t2; bEntry_to_string e2 |> print_endline; *)
				let v1 = if t1 > 1 then vars1.(e1.index) else atom_of_int t1
				and v2 = if t2 > 1 then vars2.(e2.index) else atom_of_int t2
				 in
				if Atom.equal v1 v2 then (
					(assert (Int.equal (varIndex newVars v1) (varIndex newVars v2)));
					mk (newNodes, newEntries) (varIndex newVars v1)
						~low:(app e1.low e2.low)
						~high:(app e1.high e2.high))
				else if e1.index = e1.low && e1.index = e1.high then
					mk (newNodes, newEntries) (varIndex newVars v2)
						~low:(app t1 e2.low)
						~high:(app t1 e2.high)
				else if e2.index = e2.low && e2.index = e2.high then
					mk (newNodes, newEntries) (varIndex newVars v1)
						~low:(app e1.low t2)
						~high:(app e1.high t2)
				else if (varIndex newVars v1) < (varIndex newVars v2) then
					mk (newNodes, newEntries) (varIndex newVars v1)
						~low:(app e1.low t2)
						~high:(app e1.high t2)
				else (* if (varIndex newVars v1) > (varIndex newVars v2) *)
					mk (newNodes, newEntries) (varIndex newVars v1)
						~low:(app t1 e2.low)
						~high:(app t1 e2.high)
		in Hashtbl.add memoi (t1,t2) result;
		result
	in
	let _ = app ((Hashtbl.length nodes1)-1) ((Hashtbl.length nodes2)-1) in ();
	newVars, (newNodes, newEntries)


let neg_bdd b =
	let vars, (nodes, entries) = b in
	let newNodes = Hashtbl.copy nodes
	and newEntries = Hashtbl.copy entries in
	for k=(Hashtbl.length nodes)-1 downto 2 do
		let entry = Hashtbl.find newNodes k in
		let newEntry = {
			index=entry.index;
			low=(if entry.low=0 then 1 else 0);
			high=(if entry.high=0 then 1 else 0)}
		in
		Hashtbl.replace newNodes k newEntry;
		Hashtbl.remove newEntries entry;
		Hashtbl.add newEntries newEntry k
	done;
	vars, (newNodes, newEntries)
