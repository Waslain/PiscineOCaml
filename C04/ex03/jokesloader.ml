let get_jokes filename =
	let jokes = ref [||] in
	let f = open_in filename in
	let add_pair q a =
		jokes := Array.append !jokes [| [| q; a |] |]
	in
	try
		while true do
			let line = input_line f in
			match String.split_on_char ';' line with
			| [q; a] -> add_pair q a
			| _ -> ()  (* ignore malformed lines *)
		done;
		!jokes
	with End_of_file ->
		close_in f;
		!jokes

let () =
	match Array.length Sys.argv with
	| 2 ->
		let jokes = get_jokes (Array.get Sys.argv 1) in
		(* for i = 0 to Array.length jokes - 1 do
			print_endline jokes.(i).(0);
			print_endline jokes.(i).(1)
		done *)
		Random.self_init ();
		let x = Random.int 5 in
		Printf.printf "%d\n" x;
		print_endline jokes.(x).(0);
 		print_endline jokes.(x).(1);
	| _ -> print_endline "Usage: FILENAME"
