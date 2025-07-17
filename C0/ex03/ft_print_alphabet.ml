let ft_print_alphabet () =
	let rec print2z c =
		if c <= 'z' then (
			print_char c;
			print2z (char_of_int (int_of_char c + 1))
		)
	in
	print2z 'a';
	print_char '\n'
