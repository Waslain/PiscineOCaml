let repeat_string ?opt_str:(str="x") n =
	let rec loop strb str x =
		if x > 1 then
			loop strb (str ^ strb) (x - 1)
		else
			str
	in
	if n <= (-1) then
		"Error"
	else if n = 0 then
		""
	else
		loop str str n
	
let () =
		print_endline(repeat_string (-1));
		print_endline(repeat_string 0);
		print_endline(repeat_string ~opt_str:"Toto" 1);
		print_endline(repeat_string 2);
		print_endline(repeat_string ~opt_str:"a" 5);
		print_endline(repeat_string ~opt_str:"what" 3);
