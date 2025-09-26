let () =
	print_endline "Rot by 1: abcdefghijklmnopqrstuvwxyz";
	print_endline (Ft_rot_n.ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
	print_endline "Rot by 13: abcdefghijklmnopqrstuvwxyz";
	print_endline (Ft_rot_n.ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
	print_endline "Rot by 42: 0123456789";
	print_endline (Ft_rot_n.ft_rot_n 42 "0123456789");
	print_endline "Rot by 2: OI2EAS67B9";
	print_endline (Ft_rot_n.ft_rot_n 2 "OI2EAS67B9");
	print_endline "Rot by 0: Damned !";
	print_endline (Ft_rot_n.ft_rot_n 0 "Damned !");
	print_endline "Rot by 42: (empty string)";
	print_endline (Ft_rot_n.ft_rot_n 42 "");
	print_endline "Rot by 1: NBzlk qnbjr !";
	print_endline (Ft_rot_n.ft_rot_n 1 "NBzlk qnbjr !");