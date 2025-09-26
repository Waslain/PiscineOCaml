let ft_print_comb2 () =
	let print x =
		if x < 10 then
			print_char '0';
		print_int x
	in
	let print_space () =
		print_char ',';
		print_char ' '
	in
	let print_all a b x =
		print a;
		print_char ' ';
		print b;
		if x = 1 then
			print_space ()
		else
			print_char '\n'
	in
	let rec loop a b =
		if a <> 98 then (
			print_all a b 1;
			if b = 99 then
				loop (a + 1) (a + 2)
			else
				loop a (b + 1)
		) else			
			print_all a 99 0
	in
	loop 0 1