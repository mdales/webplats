let rem_prefix base path =
  let rec loop p acc =
    match Eio.Path.split p with
    | None -> None
    | Some (path, basename) -> (
      let updated_acc = basename :: acc in
      if path = base then
        Some updated_acc
      else
        loop path updated_acc
    )
  in
  let rest = loop path [] in
  match rest with
  | None -> None
  | Some rest -> (
    let newpath = List.fold_left (fun acc p ->
      "/" ^ p ^ acc
    ) "" rest
    in Some newpath
  )