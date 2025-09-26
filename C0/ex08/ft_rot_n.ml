let ft_rot_n n str =
	let is_alpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' in
	let rot_n c =
		if is_alpha c then
			let base = if c >= 'a' then int_of_char 'a' else int_of_char 'A' in
			char_of_int (((int_of_char c - base + n) mod 26) + base)
		else
			c
	in
	String.map rot_n str
