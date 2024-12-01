type t = { path : Fpath.t; sections : Section.t list; toplevel : Section.t}

let of_directory path =
  let root_listing =
    Sys.readdir (Fpath.to_string path)
    |> Array.to_list
    |> List.map (fun p -> Fpath.append path (Fpath.v p))
  in

  let sections =
    List.filter_map
      (fun p ->
        match Sys.is_directory (Fpath.to_string p) with
        | false -> None
        | true -> Some (Fpath.to_dir_path p))
      root_listing
    |> List.map (fun p -> Section.of_directory ~base:path p)
    |> List.sort (fun a b ->
           Ptime.compare
             (Page.date (List.hd (Section.pages b)))
             (Page.date (List.hd (Section.pages a))))
  in

  let root_pages =
    List.filter_map
      (fun p ->
        match Fpath.get_ext p with ".md" -> Some (Page.of_file p) | _ -> None)
      root_listing
  in
  
  let toplevel = Section.v "website" "/" root_pages in

  { path; sections ; toplevel }

let sections t = t.toplevel :: t.sections
let path t = t.path
let title t = Fpath.basename t.path
let toplevel t = t.toplevel
