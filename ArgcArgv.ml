let rec print_list = function
	|[]			-> print_list "/n"
	|hd::tl -> print_list hd; print_string " "; print_list tl

let main ac av =
		print_list av

let () =
	let av = Array.to_list Sys.av in
	main (List.length av)	av