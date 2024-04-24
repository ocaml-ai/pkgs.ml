open Trail

let endpoint =
  [
    use (module Logger) Logger.(args ~level:Debug ());
    Router.(
      router
        [
          get "/" Home_controller.get;
          scope "/p"
            [
              get "/:source/:org/:repo/:ref/:pkg" Package_proxy_controller.proxy;
            ];
        ]);
  ]

let start_link () =
  let handler = Nomad.trail endpoint in
  Nomad.start_link ~port:8080 ~handler ()
