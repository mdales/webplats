
type t = {
	path : Fpath.t;
	pages : Page.t list;
	url : string;
} 


let rec find_markdown_files path =
	Sys.readdir (Fpath.to_string path)
	|> Array.to_list
	|> List.map (fun p -> Fpath.append path (Fpath.v p)) 
	|> List.concat_map (fun p -> 
		match Sys.is_directory (Fpath.to_string p) with
		| true -> find_markdown_files p
		| false -> (
			match Fpath.get_ext p with
			| ".md" -> [p]
			| _ -> []
		)
	)

let of_directory ~base path = 

	let url = Fpath.rem_prefix base path in
	let url = match url with
	| Some url -> "/" ^ Fpath.to_string url
	| None -> failwith "base is not parent directory"
	in

	let paths = find_markdown_files path 
	|> List.filter (fun p -> (Fpath.basename p) = "index.md" ) in
	let pages = List.map (Page.of_file ~base:base) paths
	|> List.filter (fun p -> not (Page.draft p))
	|> List.sort (fun a b -> 
		Ptime.compare (Page.date b) (Page.date a)
	) in
	{
		path ; pages ; url
	}

let title t =
	Fpath.basename t.path

let pages t = t.pages

let url t = t.url
