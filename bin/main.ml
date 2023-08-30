module First =
  struct
    let rec last list =
      match list with
        | [] -> None
        | [x] -> Some x
        | _ :: t -> last t
  end ;;


let () =
  (* first *)
  let open First in
  let first_res = last ["1" ; "2" ; "3"] in
  (* let first_res = last [] in *)
  match first_res with 
    | Some x -> print_endline x
    | None -> print_endline "nothing"
