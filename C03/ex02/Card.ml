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

Module Color =
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

