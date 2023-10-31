

let rec print_list print_elem fmt = function
  | [] -> ()
  | [x] -> print_elem fmt x 
  | x :: xs ->
      Format.fprintf fmt "%a, " print_elem x;  
      print_list print_elem fmt xs 

