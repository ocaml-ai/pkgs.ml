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
  match receive ~selector () with
  | `add_package req ->
      let _ = spawn (fun () -> handle_add_package req) in
      loop ()

and handle_add_package { source; org; repo; ref; package_name } =
  info (fun f ->
      f "Adding package %s from %s/%s/%s/%s" package_name source org repo ref);
  let _packages =
    match Github.get_file ~org ~repo ~ref ~file:"dune-project" with
    | Ok (200, contents) -> Dune_project.of_string contents
    | Ok (n, _) -> failwith (Format.sprintf "get file failed status=%d" n)
    | Error `no_data_in_file -> failwith "no data in file"
    | Error `no_status_found -> failwith "no status found"
    | _ -> failwith "other error"
  in

  Package_metrics.package_install ~source ~org ~repo ~ref;

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
  ()

let init () =
  info (fun f -> f "Initialitizing package registry");
  loop ()

let start_link () =
  let pid = spawn_link init in
  register registry_name pid;
  Ok pid

let add_package req = send_by_name ~name:registry_name (AddPackage req)
