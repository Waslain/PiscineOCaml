(* type t = Spade | Heart | Diamond | Club
val all : t list (** The list of all values of type t *)
val toString : t -> string (** "S", "H", "D" or "C" *)
val toStringVerbose : t -> string * "Spade", "Heart", etc *)
type t = Spade | Heart | Diamond | Club
let all = [Spade; Heart; Diamond; Club]

let toString = function
	| Spade -> "S" | Heart -> "H" | Diamond -> "D" | Club -> "C"
	
let toStringVerbose = function
	| Spade -> "Spade" | Heart -> "Heart" | Diamond -> "Diamond" | Club -> "Club"