open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "package_proxy_controller" ]
end)

open Trail

let ( let* ) = Result.bind

let proxy conn =
  let Package_registry.({ org; repo; ref; _ } as req) =
    Trail.Conn.
      {
        source = conn.params |> List.assoc "source";
        org = conn.params |> List.assoc "org";
        repo = conn.params |> List.assoc "repo";
        ref = conn.params |> List.assoc "ref";
        package_name = conn.params |> List.assoc "pkg";
      }
  in

  let _ = spawn (fun () -> Package_registry.add_package req) in

  let tarball_url = Github.get_ref_tarball ~org ~repo ~ref in

  conn
  |> Conn.with_header "Location" tarball_url
  |> Conn.send_response `Temporary_redirect {%b||}
