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

let () =
	let print_test c =
		Printf.printf "GenNucleotide %c  ⬇️\n" c;
		let (p, d, b) = generate_nucleotide c in
		let base_str = match b with
				| A -> "A"
				| T -> "T"
				| C -> "C"
				| G -> "G"
				| None -> "None"
		in
		Printf.printf "Phosphate: %s, Deoxyribose: %s, Base: %s\n" p d base_str;
		let expected =
			Printf.sprintf "Phosphate: phosphate, Deoxyribose: deoxyribose, Base: %s" base_str
		in
		Printf.printf "%s\nExpected ------->  ⬆️\n\n" expected
	in
	print_test 'A';
	print_test 'T';
	print_test 'C';
	print_test 'G';
	print_test 'X'
