let yaml_dict_to_string a k =
  match List.assoc_opt k a with
  | None -> None
  | Some v -> ( match v with `String str -> Some str | _ -> None)

let yaml_dict_to_string_list a k =
  match List.assoc_opt k a with
  | None -> []
  | Some v -> (
      match v with
      | `A lst ->
          List.filter_map
            (fun v -> match v with `String s -> Some s | _ -> None)
            lst
      | _ -> [])

let yaml_dict_to_string_dict a k = 
  match List.assoc_opt k a with
  | None -> []
  | Some v -> (
    match v with
    | `O assoc -> (
      List.filter_map (fun (k, v) -> 
        match v with
        | `String s -> Some (k, s)
        | _ -> None
      ) assoc
    )
    | _ -> []
  )

let yaml_dict_to_bool a k =
  match List.assoc_opt k a with
  | None -> None
  | Some v -> ( match v with `Bool b -> Some b | _ -> None)

let yaml_dict_to_date a k =
  match List.assoc_opt k a with
  | None -> None
  | Some v -> (
      match v with
      | `String str -> (
          match Ptime.of_rfc3339 str with
          | Ok (t, _, _) -> Some t
          | _ -> (
              (* Some are not RFC3339, they're '2019-09-30 08:57:22' or such *)
              try
                Scanf.sscanf str "%d-%d-%d %d:%d:%d" (fun y m d h i s ->
                    Ptime.of_date_time ((y, m, d), ((h, i, s), 0)))
              with Stdlib.Scanf.Scan_failure _ -> None))
      | _ -> None)
