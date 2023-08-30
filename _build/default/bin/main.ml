module First =
  struct
    let rec last list =
      match list with
        | [] -> None
        | [x] -> Some x
        | _ :: rest -> last rest 
  end 
;;

module Second = 
  struct
    let rec last_two list =
      match list with
        | [] -> None
        | [_] -> None
        | [x ; y] -> Some (x, y)
        | _ :: rest -> last_two rest
      
  end
;;

let () =
  (* first *)
  let open First in
  print_endline "First:" ;
  let first_res = last ["1" ; "2" ; "3"] in
  (* let first_res = last [] in *)
  match first_res with 
    | Some x -> print_endline x
    | None -> print_endline "nothing" in

  (* second *)
  let open Second in
  print_endline "Second:" ;
  let second_res = last_two ["1" ; "2" ; "3"] in
  (* let second_res = last_two ["3"] in *)
  (* let second_res = last_two [] in *)
  match second_res with 
    | Some (x , y) -> Printf.printf "%s , %s\n" x y
    | None -> print_endline "nothing"

