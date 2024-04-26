open Trail

let endpoint =
  [
    use (module Logger) Logger.(args ~level:Debug ());
    Router.(
      router
        [
          get "/" Home_controller.get;
          get "/search" Search_controller.get;
          post "/publish" Publish_controller.publish;
          get "/i/:source/:org/:repo/:ref/:pkg" Package_info_controller.get;
          scope "/p"
            [
              get "/:source/:org/:repo/:ref/:pkg" Package_proxy_controller.proxy;
            ];
        ]);
  ]

let start_link () =
  let handler = Nomad.trail endpoint in
  Nomad.start_link ~acceptors:1 ~port:8080 ~handler ()
