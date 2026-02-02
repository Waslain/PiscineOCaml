type 'a ft_ref = {mutable content : 'a}

(* return: ’a -> ’a ft_ref: creates a new reference. *)
let return x = {content = x}
(* let get = ’a ft_ref -> ’a: Dereferences a reference. *)
let get r = r.content
(* let set = ’a ft_ref -> ’a -> unit: Assigns a reference’s value. *)
let set r v = r.content <- v
(* let bind = ’a ft_ref -> (’a -> ’b ft_ref) -> ’b ft_ref: *)
let bind r f = r.content <- f r.content

let () =
	let value = return 12 in
	Printf.printf "Return test\n";
	Printf.printf "%d\n" value.content;
	Printf.printf "Set and get test\n";
	let v1 = return 1 in
	set v1 43;
	Printf.printf "%d\n" (get v1);
	Printf.printf "Bind test\n";
	let x = return 21 in
	let double a = a * 2 in
	bind x double;
	Printf.printf "%d\n" (get x);
