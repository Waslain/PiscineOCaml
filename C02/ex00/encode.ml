let rec encode (lst : 'a list) : (int * 'a) list =
  match lst with
  | [] -> [] (* Handling edge case with empty list *)
  | h :: t -> (* h is the first element, t is the last *)
      let rec count_and_rest n = function (* This subfunction help us getting n of char in the list *)
        | [] -> (n, []) (* Handle empty t *)
        | x :: xs when x = h -> count_and_rest (n + 1) xs (* if x = h then n++ *)
        | rest -> (n, rest) (* if not return the rest *)
      in
      let (cnt, rest) = count_and_rest 1 t in (* cnt total count for h and rest is the remaining list after the run *)
      (cnt, h) :: encode rest (* Make the tuple (cnt, h) to the result of c_n_r *)

(* For ["a"; "a"; "b"]:
h = "a", t = ["a"; "b"].
count_and_rest finds 1 more "a", so cnt = 2, rest = ["b"].
Result: (2, "a") :: encode ["b"].
encode ["b"] yields [(1, "b")].
Final: [(2, "a"); (1, "b")]. *)

let () = (* main function *)
  let string_of_input lst = "[" ^ String.concat "; " (List.map (fun s -> "\"" ^ s ^ "\"") lst) ^ "]" in
  let string_of_list lst = "[" ^ String.concat "; " (List.map (fun (n, s) -> Printf.sprintf "(%d, %s)" n s) lst) ^ "]" in
  let print_test name input expected =
    let result = encode input in
    Printf.printf "%s: encode %s = %s" name (string_of_input input) (string_of_list result);
    if result = expected then print_endline " (PASS)"
    else print_endline " (FAIL)"
  in
  print_test "Test 1" [] [];
  print_test "Test 2" ["a"] [(1, "a")];
  print_test "Test 3" ["a"; "a"; "b"; "b"; "b"; "c"] [(2, "a"); (3, "b"); (1, "c")]