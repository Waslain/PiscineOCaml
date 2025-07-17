let ft_string_all string =
	let is_digit c = c >= '0' && c <= '9' in
	let rec is_all_digit string i len =
		if i >= len then true
		else if is_digit (String.get string i) then
			is_all_digit string (i + 1) len
		else
			false
	in
	is_all_digit string 0 (String.length string)
