open OUnit2
open Webplats

let shortcode_printer (sc : Shortcode.t) =
  match sc with
  | Raster (a, _b, _c, _d) -> Printf.sprintf "Raster(%s, _, _, _)" a
  | Vector (a, _b, _c) -> Printf.sprintf "Vector(%s, _, _)" a
  | Audio a -> Printf.sprintf "Audio(%s)" a
  | Photo a -> Printf.sprintf "Photo(%s)" a
  | Youtube a -> Printf.sprintf "Youtube(%s)" a
  | Video (a, _b, c) -> Printf.sprintf "Video(%s, _, %b)" a c
  | Unknown a -> Printf.sprintf "Unknown(%d)" (List.length a)

let assert_equal_sc = assert_equal ~printer:shortcode_printer
let assert_equal_int = assert_equal ~printer:string_of_int

let test_invalid_empty_shortcode _ =
  let body = {| {{< >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 7 len;
  assert_equal_sc ~msg:"Code" (Shortcode.Unknown []) code

let test_invalid_shortcode _ =
  let body = {| {{< boop the snoot >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 22 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Unknown [ "boop"; "the"; "snoot" ])
    code

let test_simple_raster_shortcode _ =
  let body = {| {{< img test.jpg >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 20 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Raster ("test.jpg", None, None, None))
    code

let test_simple_vector_shortcode _ =
  let body = {| {{< img test.svg >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 20 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Vector ("test.svg", None, None))
    code

let test_multi_ext_image_shortcode _ =
  let body = {| {{< img GetPhoto-2.ashx.jpeg >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 32 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Raster ("GetPhoto-2.ashx.jpeg", None, None, None))
    code

let test_simple_image_shortcode_with_space _ =
  let body = {| {{< img "a test.jpg" >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 24 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Raster ("a test.jpg", None, None, None))
    code

let test_image_shortcode_with_alt _ =
  let body = {| {{< img test.jpg "Something else">}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 36 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Raster ("test.jpg", Some "Something else", None, None))
    code

let test_image_shortcode_with_alt_and_css_hint _ =
  let body = {| {{< img test.jpg "Something else" unrounded >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 47 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Raster ("test.jpg", Some "Something else", Some "unrounded", None))
    code

let test_invalid_image_shortcode_with_extra_param _ =
  let body = {| {{< img test.jpg "Something else" unrounded surprise >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 56 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Unknown
       [ "test.jpg"; "Something else"; "unrounded"; "surprise" ])
    code

let test_invalid_empty_image_shortcode _ =
  let body = {| {{< img >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 11 len;
  assert_equal ~msg:"Code" (Shortcode.Unknown [ "img" ]) code

let test_simple_audio_shortcode _ =
  let body = {| {{< audio test.m4a >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 22 len;
  assert_equal_sc ~msg:"Code" (Shortcode.Audio "test.m4a") code

let test_simple_youtube_shortcode _ =
  let body = {| {{< youtube dQw4w9WgXcQ >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 27 len;
  assert_equal_sc ~msg:"Code" (Shortcode.Youtube "dQw4w9WgXcQ") code

let test_simple_photo_shortcode _ =
  let body = {| {{< photo some_picture >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 26 len;
  assert_equal_sc ~msg:"Code" (Shortcode.Photo "some_picture") code

let test_simple_video_shortcode _ =
  let body = {| {{< video test.mp4 >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 22 len;
  assert_equal_sc ~msg:"Code" (Shortcode.Video ("test.mp4", None, false)) code

let test_simple_video_shortcode_with_thumbnail _ =
  let body = {| {{< video test.mp4 thumb.jpg >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 32 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Video ("test.mp4", Some "thumb.jpg", false))
    code

let test_simple_video_shortcode_looped _ =
  let body = {| {{< videoloop test.mp4 >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 26 len;
  assert_equal_sc ~msg:"Code" (Shortcode.Video ("test.mp4", None, true)) code

let test_simple_video_shortcode_with_thumbnail_looped _ =
  let body = {| {{< videoloop test.mp4 thumb.jpg >}} |} in
  let codes = Shortcode.find_shortcodes body in
  assert_equal_int ~msg:"Code count" 1 (List.length codes);
  let (loc, len), code = List.hd codes in
  assert_equal_int ~msg:"Code offset" 1 loc;
  assert_equal_int ~msg:"Code length" 36 len;
  assert_equal_sc ~msg:"Code"
    (Shortcode.Video ("test.mp4", Some "thumb.jpg", true))
    code

let suite =
  "Shortcode tests"
  >::: [
         "Test invalid empty shortcode" >:: test_invalid_empty_shortcode;
         "Test invalud shortcode" >:: test_invalid_shortcode;
         "Test simple raster shortcode" >:: test_simple_raster_shortcode;
         "Test simple vector shortcode" >:: test_simple_vector_shortcode;
         "Test multi extention image" >:: test_multi_ext_image_shortcode;
         "Test simple image shortcode with space in filename"
         >:: test_simple_image_shortcode_with_space;
         "Test image shortcode with alt" >:: test_image_shortcode_with_alt;
         "Test image shortcode with alt and css hint"
         >:: test_image_shortcode_with_alt_and_css_hint;
         "Test invalid image shortcode with extra arg"
         >:: test_invalid_image_shortcode_with_extra_param;
         "Test image shortcode invalid empty"
         >:: test_invalid_empty_image_shortcode;
         "Test simple audio shortcode" >:: test_simple_audio_shortcode;
         "Test simple youtube shortcode" >:: test_simple_youtube_shortcode;
         "Test simple photo shortcode" >:: test_simple_photo_shortcode;
          "Test simple video shortcode" >:: test_simple_video_shortcode;
          "Test simple video shortcode with thumbnail"
          >:: test_simple_video_shortcode_with_thumbnail;
           "Test simple video shortcode looped" >:: test_simple_video_shortcode_looped;
           "Test simple video shortcode with thumbnail looped"
           >:: test_simple_video_shortcode_with_thumbnail_looped;
       ]

let () = run_test_tt_main suite
