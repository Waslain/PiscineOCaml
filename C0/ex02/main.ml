let () =
  print_endline "Testing ft_power with 2 and 4";
  print_int (Ft_power.ft_power 2 4);
  print_char '\n';

  print_endline "Testing ft_power with 3 and 0";
  print_int (Ft_power.ft_power 3 0);
  print_char '\n';

  print_endline "Testing ft_power with 0 and 5";
  print_int (Ft_power.ft_power 0 5);
  print_char '\n';
