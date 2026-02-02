let jokes : string array array = [|
	[| "Pourquoi les plongeurs plongent-ils toujours en arrière ?" ; "Parce que sinon ils tombent dans le bateau." |];
	[| "Que dit le zéro au huit ?" ; "Sympa ta ceinture !" |];
	[| "Pourquoi les squelettes ne se battent-ils jamais entre eux ?" ; "Ils n'ont pas de muscles." |];
	[| "Quel est le comble pour un jardinier ?" ; "De se planter devant sa porte." |];
	[| "Avec quoi on ramasse la papaille ?" ; "Avec une foufourche." |];
|]

let () =
	Random.self_init ();
	let x = Random.int 5 in
	Printf.printf "%d\n" x;
 	print_endline jokes.(x).(0);
 	print_endline jokes.(x).(1);
