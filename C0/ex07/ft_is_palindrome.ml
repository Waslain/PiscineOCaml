let ft_is_palindrome string =
	let rec recursion_check string i len =
		if i >= len then
      true
		else if (String.get string i) = (String.get string (len -1)) then
			recursion_check string (i + 1) (len - 1)
		else
			false
	in
	recursion_check string 0 (String.length string)
