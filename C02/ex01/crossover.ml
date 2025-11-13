(* Main function that take 2 polymorphic list as args and return the elements that appear in both *)
let crossover (lst : 'a list) (lst2 : 'a list) =
	let rec cross lst res = (* Main recursion that iterate through lst while building a result list res*)
		match lst with (* Containing the elements also present in lst2 *)
		| [] -> res (* Base case, when input list is exhausted, return the accumulated result *)
		| h :: t -> (* from head to tail recursion *)
				let rec making_cross = function (* Helper function *)
					| [] -> false
					| x :: xs -> if x = h then true else making_cross xs (* h is in the scope, we don't need to pass it explicitly *)
					(* This check lst2 where if the current element h appears in lst2 and return true if any element of lst2 equals h*)
				in
				if making_cross lst2 then cross t (res @ [h]) else cross t res (* Here we pass lst2 when calling making_cross *)
				(* if h in lst2 append to res else skip, res @ [h] make a new list by concatenating res wih the element h *)
	in
	cross lst []
	(* First call of the recursion it start with an empty accumulator [] *)

let () = (* main function *)
		(* String.concat doesn't add the concatenation here the separator before the first element. Really usefull*)
		let string_of_list to_string lst = "[" ^ String.concat "; " (List.map to_string lst) ^ "]" in
		let to_string_string s = "\"" ^ s ^ "\"" in
		let to_string_int i = string_of_int i in

		let print_test to_string name input1 input2 expected =
				let result = crossover input1 input2 in
				Printf.printf "%s: %s %s = %s" name (string_of_list to_string input1) (string_of_list to_string input2) (string_of_list to_string result);
				if result = expected then print_endline " (PASS)"
				else print_endline " (FAIL)"
		in

		print_test to_string_string "Test 1" [] [] [];
		print_test to_string_string "Test 2" ["a"] ["a"] ["a"];
		print_test to_string_string "Test 3" ["a"; "b"; "c"] ["b"; "c"; "d"] ["b"; "c"];
		print_test to_string_string "Test 4" ["a"; "a"; "b"] ["a"; "c"] ["a"; "a"];
		print_test to_string_int "Test 5" [1; 2; 3] [2; 3; 4] [2; 3];
		print_test to_string_int "Test 6" [1; 1; 2] [1; 3] [1; 1];
		print_test to_string_int "Test 7" [] [1; 2] [];
		print_test to_string_int "Test 8" [1; 2] [] [];
		print_test to_string_int "Test 9" [1; 2; 2; 3] [2; 2; 4] [2; 2];
		print_test to_string_int "Test 10" [5; 4; 3; 2; 1] [1; 2; 3; 4; 5] [5; 4; 3; 2; 1]