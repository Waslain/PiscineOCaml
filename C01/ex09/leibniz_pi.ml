let leibniz_pi fl =
	if fl < 0. then -1
	else
		let pi = 4.0 *. (atan 1.0) in
		let rec loop i acc =
			let fi = float_of_int i in
			let func = ((-1.) ** fi) /. (2. *. fi +. 1.) in
			let new_acc = acc +. func in
			let abs_float = 
				let diff = new_acc *. 4. -. pi in
				if diff < 0. then -.diff else diff
			in
				if abs_float < fl then i + 1
				else loop (i + 1) new_acc
			in
			loop 0 0.0


let () =
  print_int (leibniz_pi 0.0002429851)