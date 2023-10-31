type t = string 

let reg = ref (-1)
let fresh () = 
  incr reg;
  "L" ^ (string_of_int !reg)

module M = Map.Make(String)
type 'a map = 'a M.t

module S = Set.Make(String)
type set = S.t

let pp = Format.pp_print_string

let of_string s = s




