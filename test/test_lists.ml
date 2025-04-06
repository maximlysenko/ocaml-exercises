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

let () = run_test_tt_main ("All Lists suites" >::: [ LastTests.suite; LastTwoTests.suite ])
