let sequence n =
	let rec accumulator loc count car res =
		match loc with
		| [] -> (Char.escaped car) :: (string_of_int count) :: res (* Fin de la liste *)
		| h :: t when h = car -> accumulator t (count + 1) car res (* Répétition on incrémente le compte*)
		| h :: t -> (* nouveau caractère à ajouter *)
			let new_res = (Char.escaped car) :: (string_of_int count) :: res in
			accumulator t 1 h new_res (* réinitialiser le compte à rebour à 1 h *)
	in
	let rec transform s = (* Transformation en string finale *)
		if String.length s = 0 then ""
		else
			let loc = List.of_seq (String.to_seq s) in (* conversion de string à une liste de char *)
			match loc with
			| [] -> ""
			| h :: t ->
				let resrev = accumulator t 1 h [] in (* Appel initial avec le premier char et count à 1 *)
				String.concat "" (List.rev resrev) (* Remets la liste dans l'ordre *)
	in
	if n <= 0 then ""
	else let rec loop n s =
		if n = 1 then s
		else
			let next_s = transform s in (* Appel à la fonction de transformation *)
			loop (n - 1) next_s
	in
	loop n "1"


	let () =
		print_endline (sequence (-42));
		print_endline (sequence 0);
		print_endline (sequence 1);
		print_endline (sequence 2);
		print_endline (sequence 3);
		print_endline (sequence 4);
		print_endline (sequence 5);
		print_endline (sequence 6);
		print_endline (sequence 7)