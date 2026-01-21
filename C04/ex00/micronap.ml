let my_sleep () = Unix.sleep 1

let main ac av =
	if ac <> 2 then exit 1;
	let x = (int_of_string (Array.get av 1)) in
	for i = x downto 1 do
		my_sleep()
	done

let () =
	main (Array.length Sys.argv) Sys.argv
