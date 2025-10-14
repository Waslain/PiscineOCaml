let rec iter f n x =
	if n < 0 then -1
	else if x = 0 then n
	else
		iter f (f n) (x - 1)

let () = 
	let open Printf in
	let double x = x * 2 in
	let exponent x = x * x in
	let test_iter f n x expected =
		let result = iter f n x in
		if result = expected then
			printf "Test passed: iter %d %d = %d\n" n x result
		else
			printf "Test failed: iter %d %d = %d (expected %d)\n" n x result expected
	in
	test_iter exponent 2 4 65536;
	test_iter double 2 4 32