let rec last list =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs
;;
