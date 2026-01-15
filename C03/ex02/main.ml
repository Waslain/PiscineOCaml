(*type t
val compare : t -> t -> int
val max : t -> t -> t
val min : t -> t -> t
val best : t list -> t

val isOf : t -> Color.t -> bool
val isSpade : t -> bool
val isHeart : t -> bool
val isDiamond : t -> bool
val isClub : t -> bool*)

let () =
	Printf.printf "Lets test all function\n";
	Printf.printf "Make a new card and print it\n";
	let card = Card.newCard Card.Value.King Card.Color.Heart in
	print_endline (Card.toString card);
	print_endline (Card.toStringVerbose card);
	Printf.printf "newCard, both toString and get functions are working\n";
	Printf.printf "----------------------------------------------------\n";
	Printf.printf "Now lets print all cards to show that all the list are working\n";
	let card_strings = List.map Card.toString Card.all in
	print_endline (String.concat ", " card_strings);
	Printf.printf "----------------------------------------------------\n";
	Printf.printf "Test all the compare function\n";
	Printf.printf "Compare 2H to 10D : ";
	let card2 = Card.newCard Card.Value.T2 Card.Color.Heart in
	Printf.printf "%d\n" (Card.compare card2 (Card.newCard Card.Value.T10 Card.Color.Diamond));
	Printf.printf "Compare 2D to 2D : ";
	Printf.printf "%d\n" (Card.compare card2 card2);
	Printf.printf "Compare 10H to 2D : ";
	Printf.printf "%d\n\n" (Card.compare (Card.newCard Card.Value.T10 Card.Color.Heart) card2);
	Printf.printf "Max\n";