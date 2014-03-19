type t 

val current_token : t -> Tokens.token
val next_token : t -> unit
val get_position : t -> int * int
val new_scanner : CharReader.t -> t

val main : string -> unit
