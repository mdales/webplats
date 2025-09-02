open Webplats
open OUnit2

let range_printer range_opt =
  match range_opt with
  | None -> "None"
  | Some (a, b) ->
      Printf.sprintf "Some (%d, %d)" (Int64.to_int a) (Int64.to_int b)

let test_invalid_range _ =
  let value = "random text" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" None range_opt

let test_empty_range _ =
  let value = "" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" None range_opt

let test_simple_valid_range _ =
  let value = "bytes=0-10" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" (Some (0L, 10L)) range_opt

let test_open_range_right _ =
  let value = "bytes=10-" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" (Some (10L, 41L)) range_opt

let test_open_range_left _ =
  let value = "bytes=-10" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" (Some (0L, 10L)) range_opt

let test_open_range _ =
  let value = "bytes=-" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" (Some (0L, 41L)) range_opt

let test_overlapping_excess_range _ =
  let value = "bytes=10-100" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" (Some (10L, 41L)) range_opt

let test_outside_excess_range _ =
  let value = "bytes=100-110" in
  let range_opt = Http.parse_range value 42L in
  assert_equal ~printer:range_printer ~msg:"Range" (Some (41L, 41L)) range_opt

let suite =
  "HTTP Helper tests"
  >::: [
         "Test invalid range header" >:: test_invalid_range;
         "Test empty range header" >:: test_empty_range;
         "Test simple valid range" >:: test_simple_valid_range;
         "Test open range right" >:: test_open_range_right;
         "Test open range left" >:: test_open_range_left;
         "Test open range" >:: test_open_range;
         "Test excess overlapping" >:: test_overlapping_excess_range;
         "Test excess outside" >:: test_outside_excess_range;
       ]

let () = run_test_tt_main suite
