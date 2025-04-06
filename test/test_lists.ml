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

let () = run_test_tt_main LastTests.suite
