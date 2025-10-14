let ft_sum f i x =
	if x < i then nan
	else if i <= 0 || x <= 0 then nan
	else
		let rec loop acc x =
			if x < i then acc
			else loop (acc +. f x) (x - 1)
		in
		loop 0.0 x

let () =
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10)
