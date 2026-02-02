let get_jokes filename =
	Array.set jokes [|] in
	let f
	for i = 0 to (Array.length jokes) do
		print_endline (jokes.(i).(0))
		print_endline (jokes.(i).(1))
	done


let () =
	match (Array.length Sys.argv) with
	| 2 -> get_jokes (Array.get Sys.argv 1)
	| _ -> print_endline "Usage: FILENAME"
