open Riot

module Endpoint = struct
  let trail =
    Trail.
      [
        use (module Logger) Logger.(args ~level:Debug ());
        Router.(
          router
            [
              get "/" (fun conn ->
                  conn |> Conn.send_response `OK {%b|"hello world!"|});
              scope "/p"
                [
                  get "/github.com" (fun conn ->
                      Conn.send_response `OK {%b|"none"|} conn);
                ];
            ]);
      ]

  let start_link () =
    let handler = Nomad.trail trail in
    Nomad.start_link ~port:8080 ~handler ()
end

module BazaarApp = struct
  open Supervisor

  let start () =
    Logger.set_log_level (Some Info);
    start_link ~child_specs:[ child_spec Endpoint.start_link () ] ()
end

let () = Riot.start ~apps:[ (module Riot.Logger); (module BazaarApp) ] ()
