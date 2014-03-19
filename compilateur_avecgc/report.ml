let errors = ref 0

let print (line,column) message =
  prerr_int line;
  prerr_string ",";
  prerr_int column;
  prerr_string ": ";
  prerr_string message;
  prerr_newline()

let error message = 
  incr errors;
  prerr_string message;
  prerr_newline()

let error_at_pos pos message =
  incr errors;
  print pos message

let fail message =
  error message;
  exit (-1)

let fail_at_pos pos message =
  error_at_pos pos message;
  exit (-1)

let exit () =
  exit (!errors)

let exit_on_error () =
  if !errors <> 0 then exit ()
  else ()
