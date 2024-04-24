let () = Riot.start ~apps:[ (module Riot.Logger); (module Pkgs_app) ] ()
