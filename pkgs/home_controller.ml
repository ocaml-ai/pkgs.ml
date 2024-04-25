open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "home_controller" ]
end)

open Trail

let get conn =
  let response = Package_search_index.search ~q:"" () in
  match response with
  | Ok (packages, _, _, _) ->
      let html = Template.homepage ~packages |> Html_of_jsx.render in
      conn
      |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
  | Error (`Msg m) ->
      error (fun f -> f "Failed to fetch packages: %S" m);
      conn
      |> Conn.send_response `Internal_server_error
           {%b|"Internal Server Error"::string|}
