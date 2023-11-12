open Objlng

let print_class_info class_info =
    Printf.printf "Printing class info\n";
      Printf.printf "class %s\n" class_info.class_def_info.name;
      Printf.printf "size: %d\n" class_info.size;
      Printf.printf "methods:\n";
      Env.iter (fun k v ->
        Printf.printf "%s -> %d\n" k v.offset
      ) class_info.methods;
      Printf.printf "End of class info\n"
      ;;
    let print_hash info_table =
      Printf.printf "Printing hash\n";
      Hashtbl.iter (fun k v ->
        print_class_info v;
        Printf.printf "%s -> %d\n" k v.size
      ) info_table;
      Printf.printf "End of hash\n"
      ;;
let get_class_info (prog: 'a program) (info_table: (string, class_info) Hashtbl.t) (class_name: string): class_info =
  match Hashtbl.find_opt info_table class_name with
  | Some class_info -> class_info
  | None ->
    let class_def = List.find (fun (cd: 'a class_def) -> cd.name = class_name) prog.classes in
    let class_size = 4 + 4 * List.length class_def.fields in
    let method_table = ref Env.empty in
    let next_offset = 
      let offset_counter = ref (4 * Env.cardinal !method_table) in
      fun () ->
        let current_offset = !offset_counter in
        offset_counter := !offset_counter + 4;
        current_offset
    in
    List.iter (fun (f_def: 'a function_def) ->
      let method_offset =
        match Env.find_opt f_def.name !method_table with
        | Some m_info -> m_info.offset
        | None -> next_offset ()
      in
      method_table := Env.add f_def.name { class_name = class_def.name; offset = method_offset } !method_table
    ) class_def.methods;
    let new_class_info = { size = class_size; methods = !method_table; class_def_info = class_def } in
    Hashtbl.add info_table class_def.name new_class_info;
    new_class_info


let tr_program (prog : typ program) : Imp.program = 
  
  let table_of_classes :  (string , class_info) Hashtbl.t = Hashtbl.create 16 in
    let get_class_name obj = 
    match obj.annot with
    | TClass c -> c
    | _ ->  failwith "not a class"
  in
  let size_of_class name = 
    let c = try get_class_info prog table_of_classes name with Not_found -> (
      let msg = Printf.sprintf "class %s not found\n" name in
      failwith msg)
    in
    c.size
  in
  let get_method_offset class_name method_name = 
    let c = try get_class_info prog table_of_classes class_name with Not_found -> failwith "class not found" in
    let m = try Env.find method_name c.methods with Not_found -> failwith "method not found" in
    m.offset
  in

  let get_field_offset class_name field_name =
    let c = try get_class_info prog table_of_classes class_name with Not_found -> failwith "class not found" in
    let rec aux = function
      | [] -> failwith "field not found"
      | (f, _) :: _ when f = field_name -> 4
      | _ :: tl -> 4 + aux tl
    in
    aux c.class_def_info.fields
  in




  let rec tr_expr ({annot;expr }) : Imp.expression =
    match expr with
    | Cst c -> Cst c
    | Bool b -> Bool b
    | Var v -> Var v
    | Binop (b,e1,e2) -> 
     
      let e1 = tr_expr e1 in
                         let e2 = tr_expr e2 in
                         Imp.Binop(b,e1,e2)
    | Call (s, args) -> tr_call s args 
    | MCall (obj, s, args) -> tr_mcall obj s args
    | New (s, args) -> tr_new s args
    | NewTab (t, e) -> tr_newtab t e
    | Read m -> Deref(tr_mem m)
    | This -> Var "this"

  and tr_args args = 
    let args = List.map tr_expr args in
    args
  and tr_call s args = 
    Imp.Call(s, tr_args args)

  and tr_mcall obj s args =
    match obj.expr with
    | This ->
      
      let class_name = get_class_name obj in 
      let args = (tr_expr obj) :: (tr_args args) in
      let method_name = class_name ^ "_" ^ s in
      Call(method_name, args)
    | _ ->
      let offset = get_method_offset (get_class_name obj) s in
      let tr_c = tr_expr obj in
      let args = tr_c :: (tr_args args) in
      let f = Imp.Binop(Add, Imp.Deref tr_c, Cst (4+offset)) in 
      DCall(Deref f, args)
  
  and tr_new s args =

    let size  = size_of_class s in
    Alloc (Cst size)
  and tr_newtab t e =
    let size = type_size t in
    let e = tr_expr e in
    Alloc (Binop(Mul, e, Cst size))
  and tr_mem m =
    match m with
    | Arr (e1, e2) -> 
      let e1 = tr_expr e1 in
      let e2 = tr_expr e2 in
      Imp.Binop(Add, e1, Binop(Mul, e2, Cst 4))
    | Atr (e,s) -> 
      let offset = get_field_offset (get_class_name e) s in
      let e = tr_expr e in
      Binop(Add, e, Cst offset)
  in 

  let rec tr_instruction instr : Imp.instruction = 
    match instr with
    | Putchar e -> Putchar (tr_expr e)
    | Set (v, e) -> 
      let tset = Imp.Set (v, tr_expr e) in
      (match e.expr with

       |New (class_name, args) ->
            let descr_name = class_name ^ "_" ^ "descr" in
            let write_descriptor =
              Imp.Write (Var v, Var descr_name)
            in
            let impCall = List.map tr_expr args in
            let offset = get_method_offset class_name "constructor" in
            let call_constructor =
              Imp.Expr(Imp.DCall(
              Deref(Binop(Add, Deref(Var(v)), Cst(4+offset))),
              ((Var v) :: impCall) ))
            in
            Seq [tset; write_descriptor; call_constructor]
      | _ ->tset)

    | If (e, s1, s2) -> If (tr_expr e, tr_seq s1, tr_seq s2)
    | While (e, s) -> While (tr_expr e, tr_seq s)
    | Return e -> Return (tr_expr e)
    | Expr e -> Expr (tr_expr e)
    | Write (m, e) -> Write (tr_mem m, tr_expr e)
  and tr_seq seq = 
    List.map tr_instruction seq 
    in

  let tr_function (fdef : typ Objlng.function_def) : Imp.function_def = 

    {
      name = fdef.name;
      params = List.map fst fdef.params;
      locals = List.map fst fdef.locals;
      code =  tr_seq fdef.code;
    }
  in 

  let tr_class (class_def : Objlng.typ Objlng.class_def) =
    let classname = class_def.name in
    let size = size_of_class classname in

    let descriptor_name =  Printf.sprintf "%s_descr" classname in
    let descriptor_var = Imp.Var descriptor_name in
    
    let alloc_code = Imp.Set (descriptor_name, Alloc (Cst size)) in
    let set_super_code = Imp.Write (descriptor_var, Cst 0) in
    let method_to_function (method_def : (Objlng.typ Objlng.function_def)) =
      let methodname = method_def.name in
      let new_name = Printf.sprintf "%s_%s" classname methodname in
      let new_params =
        ("this", Objlng.TClass classname) :: method_def.params
      in
      {method_def with name= new_name; params= new_params}
    in
    let method_functions = List.map method_to_function class_def.methods in
    let descr_offset n = Imp.Binop (Add, descriptor_var, Cst n) in
    let setup_methods_code =
      List.mapi
        (fun i (e : (Objlng.typ Objlng.function_def)) ->
          Imp.Write (descr_offset ((i + 1) * 4), Addr e.name) )
        method_functions
    in
    let (init_functions : Imp.function_def) =
      { name= Printf.sprintf "init_%s_" classname
      ; params= []
      ; locals= []
      ; code= [alloc_code; set_super_code] @ setup_methods_code }
    in
    (descriptor_name, init_functions, method_functions)
  in
  let classes = prog.classes in
  let class_metas = List.map tr_class classes in
  let init_functions = List.map (fun (_, y, _) -> y) class_metas in
  let method_functions =
    List.flatten (List.map (fun (_, _, z) -> z) class_metas)
  in
  let descriptor_globals = List.map (fun (x,_,_) -> x) class_metas in
  let main =
    List.find (fun (f : (Objlng.typ Objlng.function_def)) -> f.name = "main") prog.functions
  in


  let init_functions_code = List.map (fun (f : Imp.function_def) -> f.code) init_functions in
  let init_functions_code = List.flatten init_functions_code in

  let new_main =
    let main2 = tr_function main in
    {main2 with code=  init_functions_code @ main2.code }
  in
  let method_functions = List.map tr_function method_functions in
  let without_main =
    List.filter (fun (f : (Objlng.typ Objlng.function_def)) -> f.name <> "main") prog.functions
  in
  let functions =  method_functions @ List.map tr_function without_main @ [new_main] in
  let globals = descriptor_globals @ List.map fst prog.globals in
  { globals=  globals
  ; functions=functions
  }
  

