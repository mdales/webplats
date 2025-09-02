let parse_range range_str file_size =
  let rex = Pcre2.regexp {|bytes=(\d*)-(\d*)|} in
  let eof = Int64.sub file_size 1L in
  try
    let matches = Array.to_list (Pcre2.extract ~rex range_str) in
    match matches with
    | [ _; start_str; end_str ] ->
        let start_pos =
          if start_str = "" then 0L else Int64.of_string start_str
        in
        let end_pos = if end_str = "" then eof else Int64.of_string end_str in
        Some (Int64.min start_pos eof, Int64.min end_pos eof)
    | _ -> None
  with
  | Invalid_argument _ -> None
  | Not_found -> None
