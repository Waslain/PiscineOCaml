let rec ft_power n1 n2 =
  if n2 = 0 then
    1
  else
    n1 * ft_power n1 (n2 - 1)
  