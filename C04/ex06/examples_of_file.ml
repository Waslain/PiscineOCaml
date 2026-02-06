let examples_of_file (fpath : string) : (float array * string) list =
	let ic = open_in fpath in
	let rec aux acc =
		try
			let line = input_line ic in
			let line = String.trim line in
			if line = "" then aux acc
			else
				let parts = String.split_on_char ',' line |> List.map String.trim in
				match List.rev parts with
				| cls :: rev_vals ->
						let vals = List.rev rev_vals in
						let floats = Array.of_list (List.map float_of_string vals) in
						aux ((floats, cls) :: acc)
				| _ -> aux acc
		with End_of_file ->
			close_in ic;
			List.rev acc
	in
	aux []

let string_of_float_array (a : float array) : string =
	let b = Buffer.create (Array.length a * 8) in
	Buffer.add_string b "[|";
	for i = 0 to Array.length a - 1 do
		if i > 0 then Buffer.add_string b "; ";
		Buffer.add_string b (string_of_float a.(i))
	done;
	Buffer.add_string b "|]";
	Buffer.contents b

let () =
	let examples = examples_of_file "ionosphere.test.csv" in
	List.iter
		(fun (arr, cls) ->
			Printf.printf "%s, %s\n" (string_of_float_array arr) cls)
		examples
