type t = { input_char : unit -> char; 
	   mutable cchar: char option; 
	   mutable ochar: char option; 
	   mutable line: int;
	   mutable column: int }

let read_char o = 
  try Some(o.input_char ())
  with End_of_file -> None

let next_char o =
  begin
    match o.cchar with
      | None -> ()
      | Some('\n') ->
	  o.column <- 0;
	  o.line <- o.line + 1
      | _ -> 
	  o.column <- o.column + 1
  end;
  o.cchar <- read_char o
  
let current_char o = o.cchar

let get_position o = o.line,o.column

let new_charReader input_char = 
  let o = { input_char = input_char;
	    cchar = Some ' ';
	    ochar = Some ' ';
	    line = 1;
	    column = -1 }
  in 
    next_char o;
    o

let charReader_of_string s =
  let i = ref 0 and n = String.length s in
  let input_char () = 
    if !i < n then 
      let c = s.[!i] in incr i; c
    else raise End_of_file
  in
    new_charReader input_char

let charReader_of_in_channel c =
  new_charReader (function () -> Pervasives.input_char c)
