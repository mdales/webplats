open Webplats

let () =

  let site = Site.of_directory (Fpath.v "/Users/michael/Sites/mynameismwd.org/content") in


  let toplevel = [
    Dream.get "/" (fun _ -> Renderer.render_index site |> Dream.html);
    Dream.get "/css/**" (Dream.static "/Users/michael/Sites/mynameismwd.org/public/css/.");
    Dream.get "/face/**" (Dream.static "/Users/michael/Sites/mynameismwd.org/public/face/.");
  ] in
  
  let sections = List.concat_map (fun sec ->
    (
      match (Section.title sec) with
      | "snapshots" -> Dream.get (Section.url sec) (fun _ -> Snapshots.render_section sec |> Dream.html)
      | "posts" -> Dream.get (Section.url sec) (fun _ -> Posts.render_section sec |> Dream.html)
      | _ -> Dream.get (Section.url sec) (fun _ -> Renderer.render_section sec |> Dream.html)
    ) ::
    List.map (fun p -> 
      Dream.get (Page.url p) (fun _ -> Renderer.render_page p |> Dream.html)
    ) (Section.pages sec) 
  ) (Site.sections site) in
  
  Dream.run
  @@ Dream.logger
  @@ Dream.router (toplevel @ sections)
