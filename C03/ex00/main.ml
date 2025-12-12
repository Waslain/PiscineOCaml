let () =
	Printf.printf "List of card toString\n";
	List.iter (fun c -> Printf.printf "%s\n" (Color.toString c)) Color.all;
	Printf.printf "List of card toStringVerbose\n";
	List.iter (fun c -> Printf.printf "%s\n" (Color.toStringVerbose c)) Color.all;