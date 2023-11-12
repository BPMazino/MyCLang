(* 
let () =
  let c  = open_in Sys.argv.(1) in
  let lb = Lexing.from_channel c in
  let prog = Impparser.program Implexer.token lb in
  close_in c;
  ignore(Impinterp.exec_prog prog (Impinterp.VInt (int_of_string Sys.argv.(2))));
  exit 0
 *)