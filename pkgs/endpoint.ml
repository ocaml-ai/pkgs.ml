open Riot
open Trail

let endpoint =
  [
    use (module Logger) Logger.(args ~level:Debug ());
    Router.(
      router
        [
          get "/" (fun conn ->
              let html = Template.render () |> Html_of_jsx.render in
              conn |> Conn.send_response `OK {%b|html::string|});
          scope "/p"
            [
              get "/:source/:org/:repo/:ref" (fun conn ->
                  let req =
                    Package_proxy_controller.
                      {
                        source = conn.params |> List.assoc "source";
                        org = conn.params |> List.assoc "org";
                        repo = conn.params |> List.assoc "repo";
                        ref = conn.params |> List.assoc "ref";
                      }
                  in

                  let res = Package_proxy_controller.proxy req in

                  conn |> Conn.send_response `OK res);
            ];
        ]);
  ]

let start_link () =
  let handler = Nomad.trail endpoint in
  Nomad.start_link ~port:8080 ~handler ()
