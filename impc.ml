open Format

let imp2rtl imp =
  Imp2rtl.tr_program imp 
;;

let rtl2ertl rtl =
  Rtl2ertl.tr_program rtl
;;

let () =
  let file = Sys.argv.(1) in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Impparser.program Implexer.token lb in
  close_in c;

  let rtl = imp2rtl prog in
  let ertl = rtl2ertl rtl in
  (* let asm = Imp2mips.translate_program prog; *) (* Uncomment this if you need MIPS translation *)

  (* Generate RTL file *)
  let rtl_output_file = (Filename.chop_suffix file ".imp") ^ ".rtl" in
  let rtl_out = open_out rtl_output_file in
  let rtl_outf = formatter_of_out_channel rtl_out in
  Rtl.print_program rtl_outf rtl;
  pp_print_flush rtl_outf ();
  close_out rtl_out;

  (* Generate ERTL file *)
  let ertl_output_file = (Filename.chop_suffix file ".imp") ^ ".ertl" in
  let ertl_out = open_out ertl_output_file in
  let ertl_outf = formatter_of_out_channel ertl_out in
  Ertl.print_program ertl_outf ertl; 
  pp_print_flush ertl_outf ();
  close_out ertl_out;

  exit 0