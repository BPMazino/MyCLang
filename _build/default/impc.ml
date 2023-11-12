open Format;;

open Typechecker;;


let obj2imp prog : Imp.program =
  try
    let t = Typechecker.type_prog prog in
    Obj2imp.tr_program t
  with
  | Typechecker.TypeError (loc, msg) ->
    fprintf std_formatter "Type error at %s: %s\n" (loc_to_string loc) msg;
    exit 1
let imp2mips imp =
  Imp2mips.translate_program imp

let imp2llast imp =
  Imp2llast.tr_program imp
;;

let llast2rtl llast =
  Llast2rtl.tr_program llast
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


  let imp = obj2imp prog in
  let llast = imp2llast imp in
  let rtl = llast2rtl llast in
  let ertl = rtl2ertl rtl in
  
   let mips = imp2mips imp in
  
  (* let asm = Imp2mips.translate_program prog; *) (* Uncomment this if you need MIPS translation *)

    
  (* Generate IMP file *)
  let imp_output_file = (Filename.chop_suffix file ".obj") ^ ".imp" in
  let imp_out = open_out imp_output_file in
  let imp_outf = formatter_of_out_channel imp_out in
  Imppp.print_program imp_outf imp;
  pp_print_flush imp_outf ();
  
  (* Generate MIPS file *)
  let mips_output_file = (Filename.chop_suffix file ".obj") ^ ".s" in
  let mips_out = open_out mips_output_file in
  let mips_outf = formatter_of_out_channel mips_out in
  Mips.print_program mips_outf mips;
  pp_print_flush mips_outf ();

  (* Generate Llast file *)
  let llast_output_file = (Filename.chop_suffix file ".obj") ^ ".llast" in
  let llast_out = open_out llast_output_file in
  let llast_outf = formatter_of_out_channel llast_out in
  Llast.print_program llast_outf llast;
  pp_print_flush llast_outf ();
  close_out llast_out; 


  (* Generate RTL file *)
  let rtl_output_file = (Filename.chop_suffix file ".obj") ^ ".rtl" in
  let rtl_out = open_out rtl_output_file in
  let rtl_outf = formatter_of_out_channel rtl_out in
  Rtl.print_program rtl_outf rtl;
  pp_print_flush rtl_outf ();
  close_out rtl_out;

  (* Generate ERTL file *)
  let ertl_output_file = (Filename.chop_suffix file ".obj") ^ ".ertl" in
  let ertl_out = open_out ertl_output_file in
  let ertl_outf = formatter_of_out_channel ertl_out in
  Ertl.print_program ertl_outf ertl;  
  pp_print_flush ertl_outf ();
  close_out ertl_out;  
  
  exit 0