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

module Third =
  struct
    let rec nth_record list i =
      match ( list, i ) with
        | ( [], _ ) -> None
        | ( x :: _, 0 ) -> Some x
        | ( _ :: rest, index ) -> nth_record rest (index - 1)
  end
;;

module Fourth =
  struct
    let rec length_counter list count =
      match ( list, count) with
        | ( [], x ) -> x
        | (_ :: rest, x) -> length_counter rest (x + 1)

    let length list =
      length_counter list 0
  end
;;

module Fifth =
  struct
    let rec do_reverse input accumulator =
      match input with
        | [] -> accumulator
        | x :: rest -> do_reverse rest (x :: accumulator)

    let reverse list =
      do_reverse list []

  end
;;

module Sixth =
  struct
    let palindrome list = 
      let open Fifth in
      list = reverse list 

  end
;;

module Seventh =
  struct

    type 'a node =
    | One of 'a 
    | Many of 'a node list

    let flatten list =
      let rec do_flatten acc = function
        | [] -> acc
        | One x :: t -> do_flatten (x :: acc) t
        | Many l :: t -> do_flatten (do_flatten acc l) t
    in
    List.rev (do_flatten [] list);;
  end
;;

module Eight =
  struct

      let rec element_is_in_list element list =
        match list with
          | [] -> false
          | x :: rest -> if x == element 
            then true 
            else element_is_in_list element rest

      let rec do_compress list accumulator =
        match list with
          | [] -> accumulator
          | x :: rest -> if element_is_in_list x accumulator 
            then do_compress rest accumulator
            else do_compress rest (x :: accumulator)

    let compress list =
      let open Fifth in
      do_compress list [] |> reverse
  end

module Nineth =
  struct
    let rec do_pack list accumulator =
      match (list, accumulator) with 
        | ([] , _) -> accumulator
        | (current :: remaining , [] ) -> do_pack remaining ((current :: []) :: [])
        | (current :: remaining , sublist :: rest) -> if current = ( List.hd sublist )
          then do_pack remaining ((current :: sublist) :: rest) 
          else do_pack remaining (( current :: []) :: sublist :: rest)
    let pack list =
      let open Fifth in
      do_pack list [] |> reverse
  end

module Tenth =
  struct
    let rec do_pack list accumulator =
      match (list, accumulator) with
        | ([], _) -> accumulator
        | (current :: remaining, []) -> do_pack remaining ((1, current) :: [])
        | (current :: remaining , (number, character) :: rest) -> if current = character
          then do_pack remaining ((number + 1, character) :: rest) 
          else do_pack remaining ((1, current) :: (number, character) :: rest)

    let pack list =
      do_pack list []
  end


let () =
  (* first *)
  print_endline "First:" ;
  let open First in
  let result = last ["1" ; "2" ; "3"] in
  (* let first_res = last [] in *)
  let res1 = match result with
    | Some x ->  x
    | None ->  "nothing"
  in
  print_endline res1 ;

  (* second *)
  print_endline "---\nSecond:" ;
  let open Second in
  let result = last_two ["1" ; "2" ; "3"] in
  (* let second_res = last_two ["3"] in *)
  (* let second_res = last_two [] in *)
  let res2 = match result with
    | Some (x , y) -> Printf.sprintf "%s , %s" x y
    | None ->  "nothing"
  in
  print_endline res2;

  (* third *)
  print_endline "---\nThird:" ;
  let open Third in
  let result = nth_record ["1" ; "2"] 1 in
  (* let third_result = nth_record ["1" ; "2"] 2 in *)
  let res3 = match result with
    | Some x ->  x
    | None ->  "nothing"
  in
  print_endline res3;

  (* fourth *)
  print_endline "---\nFourth:" ;
  let open Fourth in
  let result = length ["1" ; "2" ; "3" ; "4"] in
  Printf.printf "%d\n" result;

  (* fifth *)
  print_endline "---\nFifth:" ;
  let open Fifth in
  let result = reverse ["1" ; "2" ; "3"] in
  List.iter (Printf.printf "%s ") result;
  print_endline "" ;

  (* sixth *)
  print_endline "---\nSixth:" ;
  let open Sixth in
  let result = palindrome ["a" ; "b" ; "a"] in
  (* let result = palindrome ["a" ; "b" ; "c"] in *)
  Printf.printf "%b\n" result;

  (* seventh *)
  print_endline "---\nSeventh:" ;
  let open Seventh in
  let result = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in
  List.iter (Printf.printf "%s ") result;

  (* eight *)
  print_endline "\n---\nEight:" ;
  let open Eight in
  let result = compress ["a" ; "a" ; "b" ; "a" ; "c" ; "c"] in
  List.iter (Printf.printf "%s ") result;

  (* nineth *)
  print_string "\n---\nNineth:" ;
  let open Nineth in
  let result = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
  (* let result = pack ["a"; "a" ; "b" ; "c" ; "c" ; "g"] in *)
  List.iter (fun res -> 
    let _ = Printf.printf "\n" in
    List.iter (fun inner -> 
      Printf.printf "%s " inner
    ) res 
  ) result;

  print_endline "\n---\nTenth:" ;
  let open Tenth in
  let result = pack ["a"; "a" ; "b" ; "c" ; "c" ; "a"] in
  List.iter (fun res -> 
    match res with
      | (num, character) -> Printf.printf "%i,%s\n" num character
  ) result


