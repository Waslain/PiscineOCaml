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
	Printf.printf "Test all the compare function\n";
	Printf.printf "Compare 2H to 10D : ";
	let card2 = Card.newCard Card.Value.T2 Card.Color.Heart in
	Printf.printf "%d\n" (Card.compare card2 (Card.newCard Card.Value.T10 Card.Color.Diamond));
	Printf.printf "Compare 2D to 2D : ";
	Printf.printf "%d\n" (Card.compare card2 card2);
	Printf.printf "Compare 10H to 2D : ";
	Printf.printf "%d\n\n" (Card.compare (Card.newCard Card.Value.T10 Card.Color.Heart) card2);
	(* Printf.printf "Max\n"; *)
	Printf.printf "Max of KH and 2D : %s\n\n" (Card.toString (Card.max card card2));
	Printf.printf "Min of KH and 2D : %s\n\n" (Card.toString (Card.min card card2));
	let cardlist = [card; card2; (Card.newCard Card.Value.T10 Card.Color.Heart)] in
	Printf.printf "Best (from list) : %s\n" (Card.toString (Card.best cardlist));
	Printf.printf "----------------------------------------------------\n";
	Printf.printf "isOf Heart %s : %b\n" (Card.toStringVerbose card) (Card.isOf card Card.Color.Heart);
	Printf.printf "isOf Spade %s : %b\n" (Card.toStringVerbose card) (Card.isOf card Card.Color.Spade);
	Printf.printf "isOf Club %s : %b\n" (Card.toStringVerbose card) (Card.isOf card Card.Color.Club);
	Printf.printf "isOf Diamond %s : %b\n" (Card.toStringVerbose card) (Card.isOf card Card.Color.Diamond);
	Printf.printf "----------------------------------------------------\n";
	Printf.printf "Same with each is'Color'\n";
	let spade = Card.newCard Card.Value.T7 Card.Color.Spade in
	let diamond = Card.newCard Card.Value.T7 Card.Color.Diamond in
	let club = Card.newCard Card.Value.T7 Card.Color.Club in
	Printf.printf "isSpade %s : %b\n" (Card.toStringVerbose spade) (Card.isSpade spade);
	Printf.printf "isSpade %s : %b\n" (Card.toStringVerbose card) (Card.isSpade card);
	Printf.printf "isHeart %s : %b\n" (Card.toStringVerbose card) (Card.isHeart card);
	Printf.printf "isHeart %s : %b\n" (Card.toStringVerbose spade) (Card.isHeart spade);
	Printf.printf "isDiamond %s : %b\n" (Card.toStringVerbose diamond) (Card.isDiamond diamond);
	Printf.printf "isDiamond %s : %b\n" (Card.toStringVerbose spade) (Card.isDiamond spade);
	Printf.printf "isClub %s : %b\n" (Card.toStringVerbose club) (Card.isClub club);
	Printf.printf "isClub %s : %b\n" (Card.toStringVerbose spade) (Card.isClub spade);
