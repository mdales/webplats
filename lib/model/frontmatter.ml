open Yamlutil

type image = {
  filename : string;
  description : string option;
  dimensions : (int * int) option;
}

type t = {
  title : string option;
  date : Ptime.t;
  synopsis : string option;
  titleimage : image option;
  draft : bool;
  tags : string list;
  images : image list;
  raw : (string * Yaml.value) list;
}

let yaml_dict_to_image a k =
  match List.assoc_opt k a with
  | None -> None
  | Some v -> (
      match v with
      | `O assoc -> (
          match yaml_dict_to_string assoc "image" with
          | Some filename ->
              Some
                {
                  filename;
                  description = yaml_dict_to_string assoc "alt";
                  dimensions = None;
                }
          | None -> None)
      | `String filename ->
          Some { filename; description = None; dimensions = None }
      | _ -> None)

(* This is complicated as older pages don't have alt text *)
let yaml_dict_to_image_list a k =
  match List.assoc_opt k a with
  | None -> []
  | Some v -> (
      match v with
      | `A lst ->
          List.filter_map
            (fun v ->
              match v with
              | `String filename ->
                  Some { filename; description = None; dimensions = None }
              | `O assoc -> (
                  match yaml_dict_to_string assoc "image" with
                  | Some filename ->
                      Some
                        {
                          filename;
                          description = yaml_dict_to_string assoc "alt";
                          dimensions = None;
                        }
                  | None -> None)
              | _ -> None)
            lst
      | _ -> [])

let yaml_to_struct y =
  match y with
  | `O assoc ->
      {
        title = yaml_dict_to_string assoc "title";
        date =
          (match yaml_dict_to_date assoc "date" with
          | Some d -> d
          | None -> Ptime.epoch);
        synopsis = yaml_dict_to_string assoc "synopsis";
        titleimage = yaml_dict_to_image assoc "titleimage";
        draft =
          (match yaml_dict_to_bool assoc "draft" with
          | Some b -> b
          | None -> true);
        tags = yaml_dict_to_string_list assoc "tags";
        images = yaml_dict_to_image_list assoc "images";
        raw = assoc;
      }
  | _ -> failwith "malformed yaml"

let of_string raw_string =
  String.trim raw_string |> Yaml.of_string_exn |> yaml_to_struct

let update_titleimage t titleimage = { t with titleimage }
let title t = t.title
let date t = t.date
let synopsis t = t.synopsis
let titleimage t = t.titleimage
let draft t = t.draft
let tags t = t.tags
let images t = t.images
let get_key_as_string t key = yaml_dict_to_string t.raw key
let get_key_as_date t key = yaml_dict_to_date t.raw key
let get_key_as_string_list t key = yaml_dict_to_string_list t.raw key
