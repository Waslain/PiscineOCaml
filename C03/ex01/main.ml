let () =
	Printf.printf "List of value toString\n";
	List.iter (fun v -> Printf.printf "%s\n" (Value.toString v)) Value.all;
	Printf.printf "List of value toStringVerbose\n";
	List.iter (fun v -> Printf.printf "%s\n" (Value.toStringVerbose v)) Value.all;
	Printf.printf "List of value toInt\n";
	List.iter (fun v -> Printf.printf "%d\n" (Value.toInt v)) Value.all;
	Printf.printf "List of value next\n";
	List.iter (fun v -> Printf.printf "%s\n" (Value.toString (Value.next v))) Value.all;
	Printf.printf "List of value previous\n";
	List.iter (fun v -> Printf.printf "%s\n" (Value.toString (Value.previous v))) (List.rev Value.all);