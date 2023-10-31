open Format

let imp2rtl imp =
  Imp2rtl.tr_program imp 
;;




let () =
  let file = Sys.argv.(1) in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Impparser.program Implexer.token lb in
  close_in c;
  let rtl = imp2rtl prog in
  let asm = Imp2mips.translate_program prog in
  (*let output_file = (Filename.chop_suffix file ".imp") ^ ".asm" in*)
  let output_file = (Filename.chop_suffix file ".imp") ^ ".rtl" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  (*Mips.print_program outf asm;*)
  Rtl.print_program outf rtl;
  pp_print_flush outf ();
  close_out out;
  exit 0
