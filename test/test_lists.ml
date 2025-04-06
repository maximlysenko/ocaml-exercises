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

let () =
  run_test_tt_main
    ("All Lists suites"
     >::: [ LastTests.suite; LastTwoTests.suite; AtTests.suite; LengthTests.suite ])
;;
