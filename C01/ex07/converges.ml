let rec iter f n x =
  if n <= 0 then
    f x = x
  else
    iter f (n - 1) (f x)
	
let () = 
	let open Printf in
	let test_iter f n x expected =
		let result = iter f n x in
		if result = expected then
			printf "Test passed: iter %d %d = %b\n" n x result
		else
			printf "Test failed: iter %d %d = %b (expected %b)\n" n x result expected
	in
	test_iter (( * ) 2) 2 5 false;
	test_iter (fun x -> x / 2) 2 3 true;
	test_iter (fun x -> x / 2) 2 2 true;