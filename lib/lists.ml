let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs
;;

let rec last_two = function
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: xs -> last_two xs
;;

let at index list =
  match List.nth list index with
  | x -> Some x
  | exception Failure _ -> None
;;

let length list =
  let rec inner acc = function
    | [] -> acc
    | _ :: xs -> inner (acc + 1) xs
  in
  inner 0 list
;;

let reverse list =
  let rec inner acc = function
    | [] -> acc
    | x :: xs -> inner (x :: acc) xs
  in
  inner [] list
;;
