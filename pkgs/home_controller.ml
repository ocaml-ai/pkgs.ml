open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "home_controller" ]
end)

open Trail

let get conn =
  (* let (packages, _, _, _) = Package_search_index.search ~q:"" () |> Result.get_ok in *)
  let packages = [] in
  let html = Template_home.make ~packages () |> Html_of_jsx.render in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
