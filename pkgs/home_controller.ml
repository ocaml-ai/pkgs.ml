open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "home_controller" ]
end)

open Trail

(*

TODO(leostera):
1. get all packages again
2. query clickhouse for stats
3. show stats on homepage

*)

let get conn =
  (* let (packages, _, _, _) = Package_search_index.search ~q:"" () |> Result.get_ok in *)
  let total_downloads = 0 in
  let total_packages = 0 in
  let latest_packages = [] in
  let html =
    Template_home.make ~latest_packages ~total_downloads ~total_packages ()
    |> Html_of_jsx.render
  in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
