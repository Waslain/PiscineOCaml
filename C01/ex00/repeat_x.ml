let repeat_x n = 
	let rec loop x str =
		if x > 0 then
			loop (x - 1) (str ^ "x")
		else
			str
	in
	if n < 0 then
		"Error"
	else if n == 0 then
		""
	else
		loop n ""

let () = 
		print_endline (repeat_x (-2));
		print_endline (repeat_x 0);
		print_endline (repeat_x 3);
		print_endline (repeat_x 5)