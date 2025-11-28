open Yamlutil

type t = {
  title : string;
  taxonomies : (string * string) list;
  base_url : Uri.t;
  hugo_theme : string;
  port : int;
  author : string option;
}

let of_file path =
  In_channel.with_open_text (Fpath.to_string path) (fun ic ->
      match In_channel.input_all ic |> Yaml.of_string_exn with
      | `O assoc ->
          let title = Option.get (yaml_dict_to_string assoc "title")
          and base_url =
            Uri.of_string (Option.get (yaml_dict_to_string assoc "baseURL"))
          and port =
            match yaml_dict_to_int assoc "port" with
            | Some p -> p
            | None -> 8080
          and hugo_theme = Option.get (yaml_dict_to_string assoc "theme")
          and author = List.assoc_opt "name" (yaml_dict_to_string_dict assoc "author")
          and taxonomies =
            yaml_dict_to_string_dict assoc "taxonomies"
            |> List.map (fun (k, v) -> (k ^ "s", v))
          in
          { title; base_url; taxonomies; hugo_theme; port; author }
      | _ -> failwith "Failed to load config")

let base_url t = t.base_url
let title t = t.title
let taxonomies t = t.taxonomies
let hugo_theme t = t.hugo_theme
let port t = t.port
let author t = t.author