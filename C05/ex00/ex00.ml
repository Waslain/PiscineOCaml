module StringMod =
	struct
		type t = string
		let compare = String.compare
	end

module StringSet = Set.Make (StringMod)

let () =
	let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
	StringSet.iter print_endline set;
	print_endline (StringSet.fold ( ^ ) set "")
