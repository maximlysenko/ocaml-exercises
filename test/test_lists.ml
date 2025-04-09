open OUnit2
open Problems.Lists

module LastTests = struct
  let suite =
    "last"
    >::: [ ("empty list" >:: fun _ -> assert_equal None (last []))
         ; ("one element" >:: fun _ -> assert_equal (Some 1) (last [ 1 ]))
         ; ("multiple elements"
            >:: fun _ -> assert_equal (Some "world") (last [ "hello"; "world" ]))
         ]
  ;;
end

module LastTwoTests = struct
  let suite =
    "last_two"
    >::: [ ("empty list" >:: fun _ -> assert_equal None (last_two []))
         ; ("one element" >:: fun _ -> assert_equal None (last_two [ 1 ]))
         ; ("two elements"
            >:: fun _ ->
            assert_equal (Some ("hello", "world")) (last_two [ "hello"; "world" ]))
         ; ("multiple elements"
            >:: fun _ -> assert_equal (Some ("b", "c")) (last_two [ "a"; "b"; "c" ]))
         ]
  ;;
end

module AtTests = struct
  let suite =
    "at"
    >::: [ ("empty list" >:: fun _ -> assert_equal None (at 0 []))
         ; ("index 2" >:: fun _ -> assert_equal (Some 3) (at 2 [ 1; 2; 3 ]))
         ; ("out of bounds" >:: fun _ -> assert_equal None (at 5 [ "a"; "b" ]))
         ]
  ;;
end

module LengthTests = struct
  let suite =
    "length"
    >::: [ ("empty list" >:: fun _ -> assert_equal 0 (length []))
         ; ("multiple elements" >:: fun _ -> assert_equal 3 (length [ 1; 2; 3 ]))
         ; ("single element" >:: fun _ -> assert_equal 1 (length [ "a" ]))
         ]
  ;;
end

module ReverseTests = struct
  let suite =
    "reverse"
    >::: [ ("empty list" >:: fun _ -> assert_equal [] (reverse []))
         ; ("single element" >:: fun _ -> assert_equal [ "a" ] (reverse [ "a" ]))
         ; ("multiple elements"
            >:: fun _ -> assert_equal [ 3; 2; 1 ] (reverse [ 1; 2; 3 ]))
         ]
  ;;
end

module IsPalindromeTests = struct
  let suite =
    "is_palindrome"
    >::: [ ("it is"
            >:: fun _ -> assert_equal true (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]))
         ; ("it is not" >:: fun _ -> assert_equal false (is_palindrome [ "a"; "b" ]))
         ]
  ;;
end

module FlattenTests = struct
  let suite =
    "flatten"
    >::: [ ("empty list" >:: fun _ -> assert_equal [] (flatten []))
         ; ("should flatten"
            >:: fun _ ->
            assert_equal
              [ "a"; "b"; "c"; "d"; "e" ]
              (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])
           )
         ]
  ;;
end

module CompressTests = struct
  let suite =
    "compress"
    >::: [ ("empty list" >:: fun _ -> assert_equal [] (compress []))
         ; ("should remove duplicates"
            >:: fun _ ->
            assert_equal
              [ "a"; "b"; "c"; "a"; "d"; "e" ]
              (compress
                 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
           )
         ]
  ;;
end

module PackTests = struct
  let suite =
    "fn pack"
    >::: [ ("should pack consecutive duplicates"
            >:: fun _ ->
            assert_equal
              [ [ "a"; "a"; "a"; "a" ]
              ; [ "b" ]
              ; [ "c"; "c" ]
              ; [ "a"; "a" ]
              ; [ "d"; "d" ]
              ; [ "e"; "e"; "e"; "e" ]
              ]
              (pack
                 [ "a"
                 ; "a"
                 ; "a"
                 ; "a"
                 ; "b"
                 ; "c"
                 ; "c"
                 ; "a"
                 ; "a"
                 ; "d"
                 ; "d"
                 ; "e"
                 ; "e"
                 ; "e"
                 ; "e"
                 ]))
         ]
  ;;
end

let () =
  run_test_tt_main
    ("All Lists suites"
     >::: [ LastTests.suite
          ; LastTwoTests.suite
          ; AtTests.suite
          ; LengthTests.suite
          ; ReverseTests.suite
          ; IsPalindromeTests.suite
          ; FlattenTests.suite
          ; CompressTests.suite
          ; PackTests.suite
          ])
;;
