open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "home_controller" ]
end)

open Trail

let total_downloads () =
  Task.async @@ fun () ->
  Clickhouse.execute Config.clickhouse_config
    "SELECT COUNT(*) FROM package_installs"
  |> Result.get_ok

let total_packages () =
  (* Task.async @@ fun () -> *)
  Package_search_index.get_total_packages ()

let get conn =
  (* let (packages, _, _, _) = Package_search_index.search ~q:"" () |> Result.get_ok in *)
  let total_downloads = total_downloads () in
  let total_packages = total_packages () in
  let total_downloads = Task.await total_downloads |> Result.get_ok in
  let total_packages = (*Task.await *) total_packages |> Result.get_ok in
  info (fun f -> f "total_downloads: %s" total_downloads);
  info (fun f -> f "total_packages: %d" total_packages);
  let latest_packages = [] in
  let html =
    Template_home.make ~latest_packages ~total_downloads ~total_packages ()
    |> Html_of_jsx.render
  in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
