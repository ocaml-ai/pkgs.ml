open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "package_proxy_controller" ]
end)

open Trail

let ( let* ) = Result.bind

type request = {
  source : string;
  org : string;
  repo : string;
  ref : string;
  package_name : string;
}

let proxy conn =
  let { source; org; repo; ref; package_name } =
    Trail.Conn.
      {
        source = conn.params |> List.assoc "source";
        org = conn.params |> List.assoc "org";
        repo = conn.params |> List.assoc "repo";
        ref = conn.params |> List.assoc "ref";
        package_name = conn.params |> List.assoc "pkg";
      }
  in

  let _dune_file =
    let* file = Github.get_file ~org ~repo ~ref ~file:"dune-project" in
    if String.length file > 0 then
      Package_search_index.(
        add_package
          {
            id =
              Printf.sprintf "%s/%s/%s/%s/%s" source org repo ref package_name;
            source;
            org;
            repo;
            ref;
            pkg = package_name;
            tags = [];
            downloads = 0L;
          })
      |> ignore;

    error (fun f -> f "dune-project: %S" file);
    Ok ()
  in
  let tarball_url = Github.get_ref_tarball ~org ~repo ~ref in

  conn
  |> Conn.with_header "Location" tarball_url
  |> Conn.send_response `Temporary_redirect {%b||}
