let fibonacci n =
	if n < 0 then
		(-1)
	else
		let rec loop n a b =
			if n = 0 then
				a
			else
				loop (n - 1) b (a + b)
		in
		loop n 0 1

let () =
	let open Printf in
	let fibonacci_not_tail n =
		if n < 0 then
			(-1)
		else
			let rec loop n =
				if n = 0 then
					0
				else if n = 1 then
					1
				else
					loop (n - 2) + loop (n - 1)
			in
			loop n
		in
	let test_fibonacci n expected =
		let result = fibonacci n in
		if result = expected then
			printf "Test passed: fibonacci %d = %d\n" n result
		else
			printf "Test failed: fibonacci %d = %d (expected %d)\n" n result expected
	in
	test_fibonacci (-42) (-1);
	test_fibonacci 1 1;
	test_fibonacci 3 2;
	test_fibonacci 6 8;
	test_fibonacci 15 610;

	(* Benchmark function *)
  let benchmark func n =
    let start = Sys.time () in
    let result = func n in
    let elapsed = Sys.time () -. start in
    let elapsed_us = elapsed *. 1_000_000.0 in
    printf "fibonacci%s %d = %d (time: %.0f microseconds)\n" (if func == fibonacci then "         " else "_not_tail") n result elapsed_us
  in

  (* Run benchmarks for small n *)
  printf "\nBenchmarking :\n";
  for n = 15 to 25 do
    benchmark fibonacci n;
		benchmark fibonacci_not_tail n
  done