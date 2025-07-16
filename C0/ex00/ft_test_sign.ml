let ft_is_positive n =
	if n >= 0 then
		true
	else
		false

let rec itoa n =
	let rec nbr n =
		if n < 10 then
			String.make 1 (Char.chr (n + 48))
		else
			nbr (n / 10) ^ String.make 1 (Char.chr ((n mod 10) + 48))
	in
	if n < 0 then
		"-" ^ nbr (-n)
	else
		nbr n

let ft_test_sign n =
	if ft_is_positive n then
		print_endline ("Test with [" ^ itoa n ^ "]: positive")
	else
		print_endline ("Test with [" ^ itoa n ^ "]: negative")