type t =
  | Video of string * string option
  | Raster of string * string option * string option * (int * int) option
  | Vector of string * string option * string option
  | Audio of string
  | Photo of string
  | Youtube of string
  | Unknown of string list

let find_raw_shortcodes body =
  let shortcode_regex = Str.regexp {|{{<\(.*\)>}}|} in
  let rec loop pos acc =
    try
      let _ = Str.search_forward shortcode_regex body pos in
      let loc = Str.match_beginning () in
      let nextpos = Str.match_end () in
      let raw = Str.matched_group 1 body in
      let shortcode = String.trim raw in
      loop nextpos ((shortcode, (loc, String.length raw + 6)) :: acc)
    with Not_found -> acc
  in
  List.rev (loop 0 [])

let img_expansion args =
  let filename = Fpath.v (List.hd args) in
  let _, ext = Fpath.split_ext filename in
  match ext with
    | ".svg" -> (
      match args with
      | [ arg1 ] -> Vector (arg1, None, None)
      | [ arg1; arg2 ] -> Vector (arg1, Some arg2, None)
      | [ arg1; arg2; arg3 ] -> Vector (arg1, Some arg2, Some arg3)
      | _ -> Unknown args
    )
    | _ -> (
    match args with
     | [ arg1 ] -> Raster (arg1, None, None, None)
     | [ arg1; arg2 ] -> Raster (arg1, Some arg2, None, None)
     | [ arg1; arg2; arg3 ] ->
         Raster (arg1, Some arg2, Some arg3, None)
     | _ -> Unknown args
    )

let find_shortcodes body =
  let rex = Pcre2.regexp {|"[^"]+"|[\w\.\-]+|} in
  find_raw_shortcodes body
  |> List.map (fun (r, loc) ->
         ( loc,
           (try Array.to_list (Pcre2.extract_all ~rex r) with Not_found -> [])
           |> List.map (fun a ->
                  let part = a.(0) in
                  match String.starts_with ~prefix:"\"" part with
                  | false -> part
                  | true -> String.sub part 1 (String.length part - 2)) ))
  |> List.map (fun (loc, sl) ->
         ( loc,
           match sl with
           | [ "video"; arg1 ] -> Video (arg1, None)
           | [ "video"; arg1; arg2 ] -> Video (arg1, Some arg2)
           | "img" :: args -> img_expansion args
           | [ "audio"; arg1 ] -> Audio arg1
           | [ "photo"; arg1 ] -> Photo arg1
           | [ "youtube"; arg1 ] -> Youtube arg1
           | _ -> Unknown sl ))
