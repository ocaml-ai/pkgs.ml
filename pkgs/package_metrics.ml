open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "package_metrics" ]
end)

let package_install ~source ~org ~repo ~ref =
  let repo_url = Github.get_repo_url ~org ~repo in
  let query =
    Format.sprintf
      {|INSERT INTO package_installs (package_source, package_org, package_repo, package_ref, package_version, package_url) VALUES ('%s', '%s', '%s', '%s', '%s', '%s')|}
      source org repo ref ref repo_url
  in
  match Clickhouse.execute Config.clickhouse_config query with
  | Ok _ -> info (fun f -> f "registered install")
  | Error `no_data_in_file -> error (fun f -> f "no data in file")
  | Error `no_status_found -> error (fun f -> f "no status found")
  | Error (#Riot.IO.io_error as e) -> error (fun f -> f "%a" Riot.IO.pp_err e)
  | _ -> error (fun f -> f "other error")
