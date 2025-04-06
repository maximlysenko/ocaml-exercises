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
