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
  | Error (#Riot.IO.io_error as e) ->
      error (fun f -> f "Failed to fetch packages: %a" Riot.IO.pp_err e);
      conn
      |> Conn.send_response `Internal_server_error
           {%b|"Internal Server Error"::string|}
  | Error
      ( `Excess_body_read | `Invalid_uri _ | `Msg _ | `Response_parsing_error
      | `Tls_error _ | `no_data_in_file ) ->
      error (fun f -> f "Failed to fetch packages: one of the other errors");
      conn
      |> Conn.send_response `Internal_server_error
           {%b|"Internal Server Error"::string|}
