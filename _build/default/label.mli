(* Label*)

(* A label is a string that is used to identify a basic block. *)

(* The type of labels *)
type t = private string 

(* Create a fresh label *)
val fresh : unit -> t


module M : Map.S with type key = t 
type 'a map  = 'a M.t

module S : Set.S with type elt = t
type set = S.t

(* Pretty-printing *)
val pp : Format.formatter -> t -> unit

