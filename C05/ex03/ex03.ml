module type FB = sig
	val bits : int
end

module type FIXED =
sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module Make (F : FB) : FIXED = struct
	type t = int
	let scale = 1 lsl F.bits
	let of_float (x : float) : t = int_of_float (x *. float_of_int scale +. 0.5)
	let of_int x = x lsl F.bits
	let to_float x = float_of_int x /. float_of_int scale
	let to_int x = int_of_float (float_of_int x /. float_of_int scale)
	let to_string x = string_of_float (to_float x)
	let zero : t = 0
	let one : t = scale
	let succ x = x + 1
	let pred x = x - 1
	let gth x y = x > y
	let lth x y = x < y
	let gte x y = x >= y
	let lte x y = x <= y
	let eqp x y = x == y (** physical equality *)
	let eqs x y = x = y (** structural equality *)
	let min x y = if gth x y then y else x
	let max x y = if gth x y then x else y
	let add x y = x + y
	let sub x y = x - y
	let mul x y = int_of_float ((float_of_int x) *. (float_of_int y) /. float_of_int scale)
	let div x y = int_of_float ((float_of_int x) /. (float_of_int y) *. float_of_int scale)
	let foreach (x : t) (y : t) (f : t -> unit) : unit =
	  let rec loop i =
	    if lte i y then begin
	      f i;
	      loop (succ i)
	    end
	  in
	  loop x
end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
	Printf.printf "=== Fixed point tests (Fixed8 with 8 fractional bits) ===\n\n";
	(* Conversions and basic ops *)
	let a8 = Fixed8.of_float 21.10 in
	let b8 = Fixed8.of_float 21.32 in
	Printf.printf "a8 (of_float 21.10) = %s  (to_float = %.6f)\n"
	(Fixed8.to_string a8) (Fixed8.to_float a8);
	Printf.printf "b8 (of_float 21.32) = %s  (to_float = %.6f)\n"
	(Fixed8.to_string b8) (Fixed8.to_float b8);

	let sum8 = Fixed8.add a8 b8 in
	Printf.printf "add: a8 + b8 = %s\n" (Fixed8.to_string sum8);

	let sub8 = Fixed8.sub b8 a8 in
	Printf.printf "sub: b8 - a8 = %s\n" (Fixed8.to_string sub8);

	let mul8 = Fixed8.mul a8 b8 in
	Printf.printf "mul: a8 * b8 = %s\n" (Fixed8.to_string mul8);

	(* Avoid dividing by zero; use a small non-zero b *)
	let div8 = Fixed8.div sum8 a8 in
	Printf.printf "div: (a8 + b8) / a8 = %s\n" (Fixed8.to_string div8);

	(* of_int and to_int *)
	let i8 = Fixed8.of_int 3 in
	Printf.printf "of_int 3 -> %s; to_int -> %d\n" (Fixed8.to_string i8) (Fixed8.to_int i8);

	(* zero, one, succ, pred *)
	Printf.printf "zero = %s, one = %s\n" (Fixed8.to_string Fixed8.zero) (Fixed8.to_string Fixed8.one);
	let s = Fixed8.succ Fixed8.zero in
	let p = Fixed8.pred Fixed8.one in
	Printf.printf "succ zero = %s, pred one = %s\n" (Fixed8.to_string s) (Fixed8.to_string p);

	(* min, max *)
	let x = Fixed8.of_float 2.5 in
	let y = Fixed8.of_float 3.1 in
	Printf.printf "x = %s, y = %s\n" (Fixed8.to_string x) (Fixed8.to_string y);
	Printf.printf "min(x,y) = %s, max(x,y) = %s\n" (Fixed8.to_string (Fixed8.min x y)) (Fixed8.to_string (Fixed8.max x y));

	(* comparisons *)
	Printf.printf "gth x y = %b\n" (Fixed8.gth x y);
	Printf.printf "lth x y = %b\n" (Fixed8.lth x y);
	Printf.printf "gte x x = %b\n" (Fixed8.gte x x);
	Printf.printf "lte x x = %b\n" (Fixed8.lte x x);

	(* equality: structural and physical *)
	let e1 = Fixed8.of_int 5 in
	let e2 = Fixed8.of_float 5.0 in
	let e3 = e1 in
	Printf.printf "e1 = %s, e2 = %s, e3 = e1\n" (Fixed8.to_string e1) (Fixed8.to_string e2);
	Printf.printf "eqs e1 e2 (structural) = %b\n" (Fixed8.eqs e1 e2);
	Printf.printf "eqp e1 e2 (physical) = %b\n" (Fixed8.eqp e1 e2);
	Printf.printf "eqp e1 e3 (physical, same binding) = %b\n" (Fixed8.eqp e1 e3);
	Printf.printf "(Note: for int-based t, physical equality behaves like structural equality for values)\n";

	(* to_int truncation check *)
	let f = Fixed8.of_float 7.9 in
	Printf.printf "of_float 7.9 = %s, to_int -> %d (truncation)\n" (Fixed8.to_string f) (Fixed8.to_int f);

	(* foreach test: print Fixed4 steps from zero to one (16 steps for 4 bits) *)
	Printf.printf "\n=== foreach iteration (Fixed4 zero..one) ===\n";
	Fixed4.foreach Fixed4.zero Fixed4.one (fun v ->
	Printf.printf "%s\n" (Fixed4.to_string v));

	Printf.printf "\nAll tests completed.\n"
