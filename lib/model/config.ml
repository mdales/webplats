open Yamlutil

type t = {
  title : string;
  taxonomies : string list;
  base_url : string;
  hugo_theme : string;
}

let of_file path =
  In_channel.with_open_text (Fpath.to_string path) (fun ic ->
      match In_channel.input_all ic |> Yaml.of_string_exn with
      | `O assoc ->
          let title = Option.get (yaml_dict_to_string assoc "title")
          and base_url = Option.get (yaml_dict_to_string assoc "baseURL")
          and hugo_theme = Option.get (yaml_dict_to_string assoc "theme")
          and taxonomies = yaml_dict_to_string_list assoc "taxonomies" in
          { title; base_url; taxonomies; hugo_theme }
      | _ -> failwith "Failed to load config")

let base_url t = t.base_url
let title t = t.title
let taxonomies t = t.taxonomies
let hugo_theme t = t.hugo_theme
