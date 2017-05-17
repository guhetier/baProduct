
module J = Yojson.Basic
module C = Cil
module F = Frontc
module E = Errormsg

let parseFile (filename: string) =
  let cabs, cil = F.parse_with_cabs filename () in
  cil

let outputFile (f : C.file) : unit =
  let output_file = "out.c" in
    try
      let c = open_out output_file in
      C.print_CIL_Input := false;
      Stats.time "printCIL"
        (C.dumpFile (!C.printerForMaincil) c output_file) f;
      close_out c
    with _ ->
      E.s (E.error "Couldn't open file %s" output_file)

let main () =
  let spec = Specification.from_file "test.spec" in
  print_string spec.Specification.ltl;
  print_newline ();
  let cilFile = parseFile "test.c" in
  Instrumentation.add_instrumentation cilFile spec;
  outputFile cilFile


let () = main()
