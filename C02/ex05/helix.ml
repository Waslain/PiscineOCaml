type phosphate = string
type deoxyribose = string
type nucleobase =  A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide (c : char ) : nucleotide =
	let base_type =
		match c with
		| 'A' -> A
		| 'T' -> T
		| 'C' -> C
		| 'G' -> G
		| _ -> None
	in
	let res : nucleotide = ("phosphate", "deoxyribose", base_type) in
	res

type helix = nucleotide list

let helix_to_string (h : helix) : string =
	let rec aux acc count = function
		| [] -> acc
		| (_, _, b)::t ->
				let base_str = match b with
					| A -> "A"
					| T -> "T"
					| C -> "C"
					| G -> "G"
					| None -> ""
				in
				if count mod 2 = 0 && count > 0 then
					aux (acc ^ base_str ^ " ") (count + 1)t
				else
					aux (acc ^ base_str) (count + 1) t
	in
	aux "" 1 h

let complementary_base (b : nucleobase) : nucleobase =
		match b with
		| A -> T
		| T -> A
		| C -> G
		| G -> C
		| None -> None

let complementary_nucleotide (nuc : nucleotide) : nucleotide =
	let (p, d, b) = nuc in
	let new_base = complementary_base b in
	(p, d, new_base)

let complementary_helix (x : helix) : helix =
	let rec loop lst =
		match lst with
		| [] -> []
		| h :: t ->
			let comp = complementary_nucleotide h in (* calcul du nucléotide pour l'élément h *)
			comp :: loop t (* Ajout du nucléotide complémentaire dans la nouvelle liste *)
		in
	loop x
		

let get_rand_base () =
	match Random.int 4 with
	| 0 -> 'A'
	| 1 -> 'T'
	| 2 -> 'C'
	| 3 -> 'G'
	| _ -> 'A'

let generate_helix (x : int) : helix =
	let n = x / 2 in
	if n <= 0 then []
	else
		let rec loop count acc =
			if count <= 0 then (* condition d'arret de la boucle *)
				acc
			else
				let new_nuc = generate_nucleotide (get_rand_base()) in (* génération du nucléotide aléatoire *)
				let comp = complementary_nucleotide new_nuc in
				loop (count - 1) (acc @ [new_nuc; comp])
		in
		loop n []
		

let () =
	Random.self_init ();
	Printf.printf "Generated Helix ⬇️\n";
	print_endline (helix_to_string (generate_helix 10))