let main () =
  Arg.parse ["-scanner",Arg.String(Scanner.main),"tests the scanner phase on the given file";
 	     "-parser",Arg.String(Parser.main false),"tests the parser phase on the given file"; 
	     "-printer",Arg.String(Parser.main true),"prints the abstract syntactical tree"; 
	     "-analyser",Arg.String(Analyser.main),"tests the analyser phase on then given file";
	     "-generate",Arg.String(Generator.main),"generate the final code";
       "-gc",Arg.Unit(Generator.gc_on),"compile with our garbage collector";
       "-list",Arg.Unit(Analyser.list_on),"compile with a default definition of the lists";
	    ]
    (fun str -> prerr_string "Ignored argument: ";prerr_string str;prerr_newline())
    "drei compiler - L3IF 2007 - ENS Lyon"

let _ = main ()
