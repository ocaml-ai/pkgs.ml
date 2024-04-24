open Riot
open Supervisor

let start () =
  Logger.set_log_level (Some Info);
  start_link ~child_specs:[ child_spec Endpoint.start_link () ] ()
