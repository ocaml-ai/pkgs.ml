open Riot
open Trail

let get conn =
  let html = Template.homepage () |> Html_of_jsx.render in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
