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

let test_invalid_date _ =
  let body = {|
date: "a long time ago"
|} in
  let fm = Frontmatter.of_string body in
  assert_equal ~msg:"Date" Ptime.epoch (Frontmatter.date fm)

let test_get_invalid_custom_key_string _ =
  let body = {|
title: "This is a test"
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_string fm "something" in
  assert_equal ~msg:"custom value" None custom_value

let test_get_empty_custom_key_string _ =
  let body = {|
something:
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_string fm "something" in
  assert_equal ~msg:"custom value" None custom_value

let test_get_custom_key_string _ =
  let body = {|
something: "This is a test"
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_string fm "something" in
  assert_equal ~msg:"custom value" (Some "This is a test") custom_value

let test_get_invalid_custom_key_date _ =
  let body = {|
title: "This is a test"
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_date fm "something" in
  assert_equal ~msg:"custom value" None custom_value

let test_get_custom_key_date _ =
  let body = {|
something: "2024-11-18 20:25:56"
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_date fm "something" in
  let expected = Ptime.of_date_time ((2024, 11, 18), ((20, 25, 56), 0)) in
  assert_equal ~msg:"custom value" expected custom_value

let test_get_empty_custom_key_date _ =
  let body = {|
something:
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_date fm "something" in
  assert_equal ~msg:"custom value" None custom_value

let test_get_custom_key_string_list _ =
  let body = {|
tags:
- here
- are
- some
- entries
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_string_list fm "tags" in
  assert_equal ~msg:"custom value"
    [ "here"; "are"; "some"; "entries" ]
    custom_value

let test_get_invalid_custom_key_string_list _ =
  let body = {|
title: "This is a test"
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_string_list fm "tags" in
  assert_equal ~msg:"custom value" [] custom_value

let test_get_empty_custom_key_string_list _ =
  let body = {|
tags:
|} in
  let fm = Frontmatter.of_string body in
  let custom_value = Frontmatter.get_key_as_string_list fm "tags" in
  assert_equal ~msg:"custom value" [] custom_value

let suite =
  "Frontmatter tests"
  >::: [
         "Empty frontmatter" >:: test_empty_fontmatter;
         "Simpe frontmatter" >:: test_simple_fontmatter;
         "RFC3339 date" >:: test_rfc3339_date;
         "Simple date" >:: test_simple_date;
         "Invalid date" >:: test_invalid_date;
         "Invalid custom string value" >:: test_get_invalid_custom_key_string;
         "Empty custom string value" >:: test_get_empty_custom_key_string;
         "Custom string value" >:: test_get_custom_key_string;
         "Invalid custom date value" >:: test_get_invalid_custom_key_date;         
         "Empty custom date value" >:: test_get_empty_custom_key_date;
         "Custom date value" >:: test_get_custom_key_date;
         "Custom string list" >:: test_get_custom_key_string_list;
         "Invalid custom string list"
         >:: test_get_invalid_custom_key_string_list;
         "Empty custom string list" >:: test_get_empty_custom_key_string_list
       ]

let () = run_test_tt_main suite
