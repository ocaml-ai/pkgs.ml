open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "package_info_controller" ]
end)

open Trail

let ( let* ) = Result.bind

let get conn =
  let package_info =
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

  let html = Template_package_info.render ~package_info |> Html_of_jsx.render in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
