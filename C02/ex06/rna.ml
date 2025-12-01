type phosphate = string
type deoxyribose = string
type nucleobase =  A | U | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide (c : char ) : nucleotide =
	let base_type =
		match c with
		| 'A' -> A
		| 'U' -> U
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
					| U -> "U"
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
		| A -> U
		| U -> A
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
	| 1 -> 'U'
	| 2 -> 'C'
	| 3 -> 'G'
	| _ -> 'A'

let generate_helix (x : int) : helix =
	if x = 1 then [generate_nucleotide (get_rand_base())]
	else let n = x / 2 in
	if n <= 0 then []
	else
		let rec loop count acc =
			if count <= 0 then (* condition d'arret de la boucle *)
				let rec merge l1 l2 =
					match l1, l2 with
					| [], _ -> l2
					| _, [] -> l1
					| h1::t1, h2::t2 -> h1 :: h2 :: merge t1 t2
				in
				if x mod 2 = 0 then
					merge acc (complementary_helix acc)
				else
					(merge acc (complementary_helix acc)) @ [generate_nucleotide (get_rand_base())]
			else
				let new_nuc = generate_nucleotide (get_rand_base()) in (* génération du nucléotide aléatoire *)
				loop (count - 1) (acc @ [new_nuc])
		in
		loop n []
	
type rna = nucleobase list

let generate_rna (hel : helix) : rna =
    let rec loop acc = function
        | [] -> acc
        | (_, _, base)::t ->
            let rna_base = complementary_base base in
            loop (acc @ [rna_base]) t
    in
    loop [] hel

let () =
    Random.self_init ();
    let rna = generate_rna (generate_helix 5) in
		let string_of_nucleobase = function
			| A -> "A"
			| U -> "U"
			| C -> "C"
			| G -> "G"
			| None -> ""
		in
		let rec print_list = function
			| [] -> ""
			| [x] -> string_of_nucleobase x
			| h::t -> string_of_nucleobase h ^ ";" ^ print_list t
		in
		print_endline ("[" ^ print_list rna ^ "]");
		let helix = List.map generate_nucleotide (List.of_seq (String.to_seq "AUCGA")) in
		let rna2 = generate_rna helix in
		print_endline ("[" ^ print_list rna2 ^ "]")