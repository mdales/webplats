open OUnit2
open Webplats

let test_empty_fontmatter _ =
  let body = "" in
  assert_raises ~msg:"Test empty frontmatter" (Failure "malformed yaml")
    (fun _ -> Frontmatter.of_string body)

let test_simple_fontmatter _ =
  let body = {|
title: "This is a test"
|} in
  let fm = Frontmatter.of_string body in
  assert_equal ~msg:"Title" (Some "This is a test") (Frontmatter.title fm)

let test_rfc3339_date _ =
  let body = {|
date: "2024-11-18T20:25:56Z"
|} in
  let fm = Frontmatter.of_string body in
  let expected =
    Option.get (Ptime.of_date_time ((2024, 11, 18), ((20, 25, 56), 0)))
  in
  assert_equal ~msg:"Date" expected (Frontmatter.date fm)

let test_simple_date _ =
  let body = {|
date: "2024-11-18 20:25:56"
|} in
  let fm = Frontmatter.of_string body in
  let expected =
    Option.get (Ptime.of_date_time ((2024, 11, 18), ((20, 25, 56), 0)))
  in
  assert_equal ~msg:"Date" expected (Frontmatter.date fm)

let suite =
  "Frontmatter tests"
  >::: [
         "Empty frontmatter" >:: test_empty_fontmatter;
         "Simpe frontmatter" >:: test_simple_fontmatter;
         "RFC3339 date" >:: test_rfc3339_date;
         "Simple date" >:: test_simple_date;
       ]

let () = run_test_tt_main suite
