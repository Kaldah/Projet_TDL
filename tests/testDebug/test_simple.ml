let%expect_test "test_simple" =
  print_endline "Hello, world!";
  [%expect {| Hello, world! |}]
