let my_sleep () = Unix.sleep 1

let main ac av =
	if ac != 2 then exit 1;
	for i = (int_of_string (Array.get av 1)) downto 1 do
		my_sleep()
	done

let () =
	let av = Array.to_list Sys.argv in
	main (List.length av) Sys.argv
