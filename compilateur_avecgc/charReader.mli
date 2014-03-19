type t

val new_charReader : (unit -> char) -> t
val next_char : t -> unit
val current_char : t -> char option
val get_position : t -> int * int

val charReader_of_string : string -> t
val charReader_of_in_channel : in_channel -> t

