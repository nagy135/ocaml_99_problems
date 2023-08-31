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
  Printf.printf "%b " result;




