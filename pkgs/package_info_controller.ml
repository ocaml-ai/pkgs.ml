open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "package_info_controller" ]
end)

open Trail

let ( let* ) = Result.bind

let get conn =
  let p =
    Trail.Conn.(
      Package_registry.
        {
          source = conn.params |> List.assoc "source";
          org = conn.params |> List.assoc "org";
          repo = conn.params |> List.assoc "repo";
          ref = conn.params |> List.assoc "ref";
          package_name = conn.params |> List.assoc "pkg";
        })
  in

  let package_id =
    Printf.sprintf "%s/%s/%s/%s/%s" p.source p.org p.repo p.ref p.package_name
  in

  let packages, _, _, _ =
    Package_search_index.search
      ~search_params:
        (Typesense.Search.make_search_params ~q:"" ~query_by:"name" ~filter_by:("id:"^package_id) ~per_page:1 ())
      ()
    |> Result.get_ok
  in

  let package_info =
    try List.hd packages
    with _ ->
      {
        id = package_id;
        source = p.source;
        org = p.org;
        repo = p.repo;
        ref = p.ref;
        name = p.package_name;
        synopsis = "ERROR: failed to load";
        description = "ERROR: failed to load";
        tags = [];
        downloads = 0L;
      }
  in

  let html = Template_package_info.render ~package_info |> Html_of_jsx.render in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
