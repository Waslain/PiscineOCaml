let ackermann m n =
	if m < 0 || n < 0 then
		-1
	else
	let rec loop m n =
		if m = 0 then
			n + 1
		else if n = 0 then
			loop (m - 1) 1
		else
			loop (m - 1) (loop m (n - 1))
	in
		loop m n
		
let () =
	print_int (ackermann (-1) 7);
	print_endline "";
	print_int (ackermann 10 (-21));
	print_endline "";
	print_int (ackermann 0 0);
	print_endline "";
	print_int (ackermann 2 3);
	print_endline "";
	print_int (ackermann 4 1)