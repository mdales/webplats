open OUnit2
open Webplats

let test_simple_page _ =
  let frontmatter = Frontmatter.of_string {|
title: test
|} in
  let body = {|Hello, world|} in
  let page =
    Page.v "section" "/section/"
      (Fpath.v "/home/test/site/section/page/index.md")
      frontmatter body
  in
  assert_equal ~msg:"Title" "test" (Page.title page);
  assert_equal ~msg:"section title" "section" (Page.original_section_title page);
  assert_equal ~msg:"section url" "/section/" (Page.original_section_url page);
  assert_equal ~msg:"body" "Hello, world" (Page.body page);
  assert_equal ~msg:"shortcodes" [] (Page.shortcodes page);
  assert_equal ~msg:"url name" "page" (Page.url_name page)

let test_non_index_name _ =
  let frontmatter = Frontmatter.of_string {|
title: test
|} in
  let body = {|Hello, world|} in
  let page =
    Page.v "section" "/section/"
      (Fpath.v "/home/test/site/section/page/about.md")
      frontmatter body
  in
  assert_equal ~msg:"url name" "about" (Page.url_name page)

let test_dir_path_with_base _ =
  let frontmatter = Frontmatter.of_string {|
title: test
|} in
  let body = {|Hello, world|} in
  let page =
    Page.v
      ~base:(Some (Fpath.v "/home/test/site/section/"))
      "section" "/section/"
      (Fpath.v "/home/test/site/section/page/with/parts/index.md")
      frontmatter body
  in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:"url name" "page/with/parts" (Page.url_name page)

let test_file_path_with_base _ =
  let frontmatter = Frontmatter.of_string {|
title: test
|} in
  let body = {|Hello, world|} in
  let page =
    Page.v
      ~base:(Some (Fpath.v "/home/test/site/section/"))
      "section" "/section/"
      (Fpath.v "/home/test/site/section/page/with/parts/about.md")
      frontmatter body
  in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:"url name" "page/with/parts/about" (Page.url_name page)

let suite =
  "Page tests"
  >::: [
         "Simple page" >:: test_simple_page;
         "Non index name" >:: test_non_index_name;
         "Index.md page with base" >:: test_dir_path_with_base;
         "Named.md page with base" >:: test_file_path_with_base;
       ]

let () = run_test_tt_main suite
