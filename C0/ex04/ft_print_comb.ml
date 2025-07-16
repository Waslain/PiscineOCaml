let ft_print_comb () =
  let rec first_num n1 =
    if n1 <= 7 then (
      let rec second_num n2 =
        if n2 <= 8 then (
          let rec third_num n3 =
            if n3 <=9 then (
              if n1 = 7 && n2 = 8 && n3 = 9 then (
                print_int (n1 * 100 + n2 * 10 + n3);
                print_char '\n'
              ) else (
                if n1 = 0 then
                  print_string "0";
                print_int (n1 * 100 + n2 * 10 + n3);
                print_string ", ";
                third_num (n3 + 1)
              )
            )
          in
          third_num (n2 + 1);
          second_num (n2 + 1)
        )
      in
      second_num (n1 + 1);
      first_num (n1 + 1)
    )
  in
  first_num 0
