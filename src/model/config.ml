open Yamlutil

type 'a t = {
  title : string;
  taxonomies : (string * string) list;
  base_url : Uri.t;
  hugo_theme : string;
  port : int;
  author : string option;
  css : 'a Eio.Path.t option;
}
  constraint 'a = [> Eio.Fs.dir_ty ]

let of_file path =
  Eio.Path.with_open_in path (fun file ->
      let content = Eio.Flow.read_all file in
      match Yaml.of_string_exn content with
      | `O assoc ->
          let title = Option.get (yaml_dict_to_string assoc "title")
          and base_url =
            Uri.of_string (Option.get (yaml_dict_to_string assoc "baseURL"))
          and port =
            match yaml_dict_to_int assoc "port" with
            | Some p -> p
            | None -> 8080
          and hugo_theme = Option.get (yaml_dict_to_string assoc "theme")
          and author =
            List.assoc_opt "name" (yaml_dict_to_string_dict assoc "author")
          and taxonomies =
            yaml_dict_to_string_dict assoc "taxonomies"
            |> List.map (fun (k, v) -> (k ^ "s", v))
          and relative_css = yaml_dict_to_string assoc "css" in
          let css =
            match relative_css with
            | None -> None
            | Some p ->
                let parent, _ = Option.get (Eio.Path.split path) in
                Some Eio.Path.(parent / p)
          in
          { title; base_url; taxonomies; hugo_theme; port; author; css }
      | _ -> failwith "Failed to load config")

let base_url t = t.base_url
let title t = t.title
let taxonomies t = t.taxonomies
let hugo_theme t = t.hugo_theme
let port t = t.port
let author t = t.author
let css_path t = t.css
