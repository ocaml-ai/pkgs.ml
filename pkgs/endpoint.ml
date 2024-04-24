open Riot
open Trail

let endpoint =
  [
    use (module Logger) Logger.(args ~level:Debug ());
    Router.(
      router
        [
          get "/" (fun conn ->
              conn |> Conn.send_response `OK {%b|"hello world!"|});
          scope "/p"
            [
              get "/:source/:org/:repo/:ref" (fun conn ->
                  let source = conn.params |> List.assoc "source" in
                  let org = conn.params |> List.assoc "org" in
                  let repo = conn.params |> List.assoc "repo" in
                  let ref = conn.params |> List.assoc "ref" in
                  let slash = "/" in
                  Conn.send_response `OK
                    {%b|
                      source::string,
                      slash::string,
                      org::string,
                      slash::string,
                      repo::string,
                      slash::string,
                      ref::string
                    |}
                    conn);
            ];
        ]);
  ]

let start_link () =
  let handler = Nomad.trail endpoint in
  Nomad.start_link ~port:8080 ~handler ()
