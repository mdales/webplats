type image = {
	filename : string;
	description : string option;
}

type frontmatter = {
	title: string option;
	date: Ptime.t;
	synopsis: string option;
	titleimage: image option;
	draft: bool;
}

type t = {
	frontmatter : frontmatter;
	url : string;
}

let yaml_dict_to_string a k = 
	match List.assoc_opt k a with
	| None -> None
	| Some v -> (
		match v with
		| `String str -> Some str
		| _ -> None
		)

let yaml_dict_to_bool a k = 
	match List.assoc_opt k a with
	| None -> None
	| Some v -> (
		match v with
		| `Bool b -> Some b
		| _ -> None
		)

let yaml_dict_to_date a k =
	match List.assoc_opt k a with
	| None -> None
	| Some v -> (
		match v with
		| `String str -> (
			match (Ptime.of_rfc3339 str) with
			| Ok (t, _, _) -> Some t
			| _ -> (
				(* Some are not RFC3339, they're '2019-09-30 08:57:22' or such *)
				Scanf.sscanf str "%d-%d-%d %d:%d:%d" (fun y m d h i s -> Ptime.of_date_time ((y, m, d), ((h, i, s), 0)))
			)
		)
		| _ -> None
	)

let yaml_dict_to_image a k = 
	match List.assoc_opt k a with
	| None -> None
	| Some v -> (
		match v with 
		| `O assoc -> (
			match yaml_dict_to_string assoc "image" with
			| Some filename -> Some {
				filename ;
				description = yaml_dict_to_string assoc "alt"
			}
			| None -> None
			
		)
		| `String filename -> (
			Some { filename ; description=None }
		)
		| _ -> None
	)

let yaml_to_struct y =
	match y with
	| `O assoc -> (
		{
			title = yaml_dict_to_string assoc "title";
			date = (match (yaml_dict_to_date assoc "date") with Some d -> d | None -> Ptime.epoch);
			synopsis = yaml_dict_to_string assoc "synopsis";
			titleimage = yaml_dict_to_image assoc "titleimage";
			draft = (match (yaml_dict_to_bool assoc "draft") with Some b -> b | None -> true);
		}
	)
	| _ -> failwith "malformed yaml"

let read_frontmatter path = 
	let raw_frontmatter = In_channel.with_open_text (Fpath.to_string path) (fun ic ->
		let parts = In_channel.input_all ic |> Astring.String.cuts ~sep:"---" in
		match parts with
		| _ :: fm :: _body -> fm
		| _ -> failwith (Printf.sprintf "failed to parse %s" (Fpath.to_string path))
	) in
	let frontmatter = String.trim raw_frontmatter |> Yaml.of_string_exn in
	yaml_to_struct frontmatter
	

let of_file ~base path = 
	let url = Fpath.rem_prefix base path in
	let frontmatter = try
		 read_frontmatter path
	with
	| Not_found | Invalid_argument _ -> failwith (Printf.sprintf "Failed to find key in %s" (Fpath.to_string path))
	in
	
	match url with
	| Some url ->{ frontmatter ; url="/" ^ Fpath.to_string url}
	| None -> failwith "base is not parent directory"

let title t = 
	match t.frontmatter.title with
	| Some t -> t
	| None -> "Untitled"
	
let url t = t.url

let date t = t .frontmatter.date

let synopsis t = t.frontmatter.synopsis

let titleimage t = t.frontmatter.titleimage

let draft t = t.frontmatter.draft

