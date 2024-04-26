open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "search_controller" ]
end)

open Trail

let get conn =
  let q = Trail.Conn.(conn.req.query) |> List.assoc "q" |> List.hd in
  let packages, found, _, _ =
    Package_search_index.search ~q () |> Result.get_ok
  in
  let html = Template_search.render ~found ~q ~packages |> Html_of_jsx.render in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
