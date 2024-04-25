open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "package_registry" ]
end)

let ( let* ) = Result.bind
let registry_name = "Pkgs.PackageRegistry"

type add_package_request = {
  source : string;
  org : string;
  repo : string;
  ref : string;
  package_name : string;
}

type Message.t += AddPackage of add_package_request

let rec loop () =
  let selector msg =
    match msg with AddPackage req -> `select (`add_package req) | _ -> `skip
  in
  match receive ~selector () with `add_package req -> handle_add_package req

and handle_add_package { source; org; repo; ref; package_name } =
  info (fun f ->
      f "Adding package %s from %s/%s/%s/%s" package_name source org repo ref);
  let file =
    Github.get_file ~org ~repo ~ref ~file:"dune-project" |> Result.get_ok
  in

  let () =
    let repo_url = Github.get_repo_url ~org ~repo in
    let query =
      Format.sprintf
        {|INSERT INTO package_installs (package_source, package_org, package_repo, package_ref, package_version, package_url) VALUES ('%s', '%s', '%s', '%s', '%s', '%s')|}
        source org repo ref ref repo_url
    in
    match Clickhouse.execute Config.clickhouse_config query with
    | Ok _ -> info (fun f -> f "registered install")
    | Error _e -> error (fun f -> f "something went wrong")
  in

  let _ =
    Package_search_index.(
      add_package
        {
          id = Printf.sprintf "%s/%s/%s/%s/%s" source org repo ref package_name;
          source;
          org;
          repo;
          ref;
          pkg = package_name;
          synopsis = "";
          description = "";
          tags = [];
          downloads = 0L;
        })
    |> Result.get_ok
  in
  error (fun f -> f "dune-project: %S" file);
  loop ()

let init () =
  info (fun f -> f "Initialitizing package registry");
  loop ()

let start_link () =
  let pid = spawn_link init in
  register registry_name pid;
  Ok pid

let add_package req = send_by_name ~name:registry_name (AddPackage req)
