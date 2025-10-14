let tak x y z = 
	let rec loop x y z =
		if y < x then
			loop (loop (x - 1) y z) (loop (y - 1) z x) (loop (z - 1) x y)
		else
			z
	in
	loop x y z

let () =
	let open Printf in
	let test_tak x y z expected =
		let result = tak x y z in
		if result = expected then
			printf "Test passed: tak %d %d %d = %d\n" x y z result
		else
			printf "Test failed: tak %d %d %d = %d (expected %d)\n" x y z result expected
	in
	test_tak 1 2 3 3;
	test_tak 5 23 7 7;
	test_tak 9 1 0 1;
	test_tak 1 1 1 1;
	test_tak 0 42 0 0;
	test_tak 23498 98734 98776 98776
	