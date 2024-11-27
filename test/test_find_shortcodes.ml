open OUnit2
open Webplats

let test_simple_image_shortcode _ =
  let body = {| {{< img test.jpg >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal ~msg:"Code offset" 1 loc;
  assert_equal ~msg:"Code length" 20 len;
  assert_equal ~msg:"Code" (Shortcode.Image ("test.jpg", None, None)) code

let test_image_shortcode_with_alt _ =
  let body = {| {{< img test.jpg "Something else">}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal ~msg:"Code offset" 1 loc;
  assert_equal ~msg:"Code length" 36 len;
  assert_equal ~msg:"Code"
    (Shortcode.Image ("test.jpg", Some "Something else", None))
    code

let suite =
  "Shortcode tests"
  >::: [
         "Test simple image shortcode" >:: test_simple_image_shortcode;
         "Test image shortcode with alt" >:: test_image_shortcode_with_alt;
       ]

let () = run_test_tt_main suite
