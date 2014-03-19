val error : string -> unit
val error_at_pos : int * int -> string -> unit
val fail : string -> 'a
val fail_at_pos : int * int -> string -> 'a
val exit : unit -> 'a
val exit_on_error : unit -> unit
