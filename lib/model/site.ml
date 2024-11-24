type t = { path : Fpath.t; sections : Section.t list }

let of_directory path =
  let sections =
    Sys.readdir (Fpath.to_string path)
    |> Array.to_list
    |> List.map (fun p -> Fpath.append path (Fpath.v p))
    |> List.filter_map (fun p ->
           match Sys.is_directory (Fpath.to_string p) with
           | false -> None
           | true -> Some (Fpath.to_dir_path p))
    |> List.map (fun p -> Section.of_directory ~base:path p)
    |> List.sort (fun a b ->
           Ptime.compare
             (Page.date (List.hd (Section.pages b)))
             (Page.date (List.hd (Section.pages a))))
  in
  Dream.log " %d\n" (List.length sections);
  { path; sections }

let sections t = t.sections
let path t = t.path
let title t = Fpath.basename t.path
