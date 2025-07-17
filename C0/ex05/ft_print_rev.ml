let	ft_print_rev string =
	let len = (String.length string) - 1 in
	let rec revprint len string =
		if len >= 0 then (
			print_char (String.get string len);
			revprint (len - 1) string
		)
	in
	revprint len string;
	print_char '\n'