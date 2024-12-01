open OUnit2
open Webplats

let test_simple_page _ =
  let frontmatter = Frontmatter.of_string {|
title: test
|} in
  let body = {|Hello, world|} in
  let page =
    Page.v (Fpath.v "/home/test/site/section/page/index.md") frontmatter body
  in
  assert_equal ~msg:"Title" "test" (Page.title page);
  assert_equal ~msg:"body" "Hello, world" (Page.body page);
  assert_equal ~msg:"shortcodes" [] (Page.shortcodes page);
  assert_equal ~msg:"url name" "page" (Page.url_name page)

let test_non_index_name _ =
  let frontmatter = Frontmatter.of_string {|
title: test
|} in
  let body = {|Hello, world|} in
  let page =
    Page.v (Fpath.v "/home/test/site/section/page/about.md") frontmatter body
  in
  assert_equal ~msg:"url name" "about" (Page.url_name page)

let suite =
  "Page tests"
  >::: [
         "Simple page" >:: test_simple_page;
         "Non index name" >:: test_non_index_name;
       ]

let () = run_test_tt_main suite
