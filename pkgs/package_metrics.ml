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
  | Error _e -> error (fun f -> f "something went wrong")
