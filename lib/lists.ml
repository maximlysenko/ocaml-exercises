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

let is_palindrome list = reverse list = list

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec inner acc = function
    | [] -> acc
    | One node :: xs -> inner (node :: acc) xs
    | Many nodes :: xs -> inner acc (nodes @ xs)
  in
  reverse (inner [] list)
;;

let compress list =
  let rec inner result = function
    | [] -> result
    | a :: [] -> a :: result
    | a :: (b :: _ as tail) ->
      if a = b then inner result tail else inner (a :: result) tail
  in
  reverse (inner [] list)
;;

let pack list =
  let rec inner current result = function
    | [] -> []
    | [ a ] -> (a :: current) :: result
    | a :: (b :: _ as tail) ->
      if a = b
      then inner (a :: current) result tail
      else inner [] ((a :: current) :: result) tail
  in
  reverse (inner [] [] list)
;;
