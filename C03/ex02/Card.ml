(*We have colors and values; now we can have cards! Write the file Card.ml that adheres
to the interface below. Several points to note regarding this interface:
• The Card module embeds the Color and Value modules. Just copy your previous
code in the corresponding structures.
• The type Card.t is abstract. That means you’re free to implement it as you want.
Choose wisely, some solutions are better than otters. And otters are cute.
• All values’ and functions’ types and identifiers are self explainatory. Just read and
use your brain, no tricks here.
• The function toString : t -> string returns strings like: "2S", "10H", "KD",
...
• The function toStringVerbose : t -> string returns strings like: "Card(7,
Diamond)", "Card(Jack, Club)", "Card(As, Spade)", ...
• The function compare : t -> t -> int behaves like the Pervasives compare
function.
• The functions max and min return the first parameter if the two cards are equal.
• The function best : t list -> t calls invalid_arg if the list is empty. If two or
more cards are equal in value, return the first one. True coders use List.fold_left
to do this function.*)

module Color =
struct
	type t = Spade | Heart | Diamond | Club
	let all = [Spade; Heart; Diamond; Club]

	let toString = function
		| Spade -> "S" | Heart -> "H" | Diamond -> "D" | Club -> "C"
		
	let toStringVerbose = function
		| Spade -> "Spade" | Heart -> "Heart" | Diamond -> "Diamond" | Club -> "Club"
end

module Value =
struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt = function
			| T2 -> 1 | T3 -> 2 | T4 -> 3 | T5 -> 4 | T6 -> 5 | T7 -> 6 | T8 -> 7 | T9 -> 8 | T10 -> 9 | Jack -> 10 | Queen -> 11 | King -> 12 | As -> 13

	let toString = function
			| T2 -> "2" | T3 -> "3" | T4 -> "4" | T5 -> "5" | T6 -> "6" | T7 -> "7" | T8 -> "8" | T9 -> "9" | T10 -> "10" | Jack -> "J" | Queen -> "Q" | King -> "K" | As -> "A"

	let toStringVerbose = function
			| T2 -> "2" | T3 -> "3" | T4 -> "4" | T5 -> "5" | T6 -> "6" | T7 -> "7" | T8 -> "8" | T9 -> "9" | T10 -> "10" | Jack -> "Jack" | Queen -> "Queen" | King -> "King" | As -> "As"

	let next = function
			| T2 -> T3
			| T3 -> T4
			| T4 -> T5
			| T5 -> T6
			| T6 -> T7
			| T7 -> T8
			| T8 -> T9
			| T9 -> T10
			| T10 -> Jack
			| Jack -> Queen
			| Queen -> King
			| King -> As
			| As -> invalid_arg "next: As has no next"

	let previous = function
			| T2 -> invalid_arg "previous: T2 has no previous"
			| T3 -> T2
			| T4 -> T3
			| T5 -> T4
			| T6 -> T5
			| T7 -> T6
			| T8 -> T7
			| T9 -> T8
			| T10 -> T9
			| Jack -> T10
			| Queen -> Jack
			| King -> Queen
			| As -> King
end

type t = Value.t * Color.t

let newCard value color = (value, color)

let allSpades = List.map (fun v -> v, Color.Spade) Value.all
let allHearts = List.map (fun v -> v, Color.Heart) Value.all
let allDiamonds = List.map (fun v -> v, Color.Diamond) Value.all
let allClubs = List.map (fun v -> v, Color.Club) Value.all
let all = List.concat [allSpades; allHearts; allDiamonds; allClubs]

let getValue (value, _) = value
let getColor (_, color) = color

let toString t = Printf.sprintf "%s%s" (Value.toString (getValue t)) (Color.toString (getColor t))
let toStringVerbose t = Printf.sprintf "Card (%s, %s)" (Value.toStringVerbose (getValue t)) (Color.toStringVerbose (getColor t))

let compare a b =
	let va = Value.toInt (getValue a) in
	let vb = Value.toInt (getValue b) in
	if va = vb then 0
	else if va < vb then -1
	else 1

let max a b = 
	let va = Value.toInt (getValue a) in
	let vb = Value.toInt (getValue b) in
	if va = vb then a
	else if va < vb then b
	else a
let min a b = 
	let va = Value.toInt (getValue a) in
	let vb = Value.toInt (getValue b) in
	if va = vb then a
	else if va < vb then a
	else b
let best (lst : t list) = 
	if lst = [] then invalid_arg "best: empty list"
	else List.fold_left (fun acc x -> if compare acc x >= 0 then acc else x) (List.hd lst) (List.tl lst)

let isOf t color = getColor t = color 
let isSpade t = t = Color.Spade 
let isHeart t = Color.Heart
let isDiamonds t = Color.Diamond
let isClubs t = Color.Club