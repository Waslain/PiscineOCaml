let gray n =
  if n <= 0 then (* handling edge case *)
    print_newline ()
  else
		(* The limit of our loop is 2^n numbers *)
    let limit = 1 lsl n in (* lsl is a bitwise left shift to avoid making pow2 function *)
    let rec loop i =
      if i = limit then
        print_newline () (* Final number done we print a new line *)
      else
				(* Making the grey code *)
        let g = i lxor (i lsr 1) in (* lsr is right shift and lxor is bitwise exclusive-or*)
				(* i from 0..2^n-1, g give use the Gray-Code sequence *)
        let code = (* code will represent the Gray-Code integer into a string *)
          String.init n (fun idx -> (* Here we build a string of size n using a helper function *)
              if ((g lsr (n - 1 - idx)) land 1) = 0 then '0' else '1') (* Code is n-length with g*)
							(* (n - 1 - idx) select the bit position -> g lsr bit extract the bit 0 or 1 *)
							(* land is the bitwise AND operator, land 1 extracts the rightmost bit as either 0 or 1 *)
							(*g = 101
								g lsr 1 = 010  (shift right by 1)
								010 land 1 = 0  (extract rightmost bit)*)
        in
        if i > 0 then print_char ' ';
        print_string code;
        loop (i + 1)
    in
    loop 0

let () =
  let print_test n expected =
    Printf.printf "gray⬇ %d  ⬇️\n" n;
    gray n;
    Printf.printf "%s\nExpected ⬆️\n\n" expected
  in
  print_test 1 "0 1";
  print_test 2 "00 01 11 10";
  print_test 3 "000 001 011 010 110 111 101 100";
  print_test 0 "";
  print_test (-1) ""