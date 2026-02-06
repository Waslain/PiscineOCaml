let sum x y = x +. y

let () =
	Printf.printf "%f\n" (sum 10.0156 5.51);
	Printf.printf "%f\n" (sum 2. 1.);
	Printf.printf "%f\n" (sum 2. (-1.));
	Printf.printf "%f\n" (sum 1. (-4.))
