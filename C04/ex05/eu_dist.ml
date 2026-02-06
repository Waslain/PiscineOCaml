let eu_dist a b =
	let res = ref 0. in
	let sqr x = x *. x in
	for i = 0 to (Array.length a - 1) do
		res := !res +. (sqr (a.(i) -. b.(i)))
	done;
	sqrt !res

let () =
	let a = [|0.0; 3.0; 4.0|] in
	let b = [|0.0; 0.0; 0.0|] in
	let d = eu_dist a b in
	Printf.printf "eu_dist between [|0.;3.;4.|] and [|0.;0.;0.|] = %f\n" d;
	if abs_float (d -. 5.0) < 1e-9 then
	 Printf.printf "test passed\n"
	else
		(
			Printf.printf "test FAILED: expected 5.0\n";
			exit 1
		 )
