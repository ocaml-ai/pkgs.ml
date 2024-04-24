open Riot
open Trail

let endpoint =
  [
    use (module Logger) Logger.(args ~level:Debug ());
    Router.(
      router
        [
          get "/" (fun conn ->
              let html = Template.homepage () |> Html_of_jsx.render in
              conn
              |> Conn.send_response `OK
                   {%b|"<!doctype html>"::string,html::string|});
          scope "/p"
            [
              get "/:source/:org/:repo/:ref/:pkg" Package_proxy_controller.proxy;
            ];
        ]);
  ]

let start_link () =
  let handler = Nomad.trail endpoint in
  Nomad.start_link ~port:8080 ~handler ()
