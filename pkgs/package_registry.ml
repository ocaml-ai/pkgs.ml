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

type Message.t +=
  | AddPackage of add_package_request * unit Ref.t * Pid.t
  | PackageAdded of unit Ref.t

let rec loop () =
  let selector msg =
    match msg with
    | AddPackage (req, ref, reply) -> `select (`add_package (req, ref, reply))
    | _ -> `skip
  in
  match receive ~selector () with
  | `add_package (req, ref, reply) ->
      let _ = spawn (fun () -> handle_add_package req ref reply) in
      loop ()

and handle_add_package { source; org; repo; ref; package_name } msg_ref reply =
  info (fun f ->
      f "Adding package %s from %s/%s/%s/%s" package_name source org repo ref);
  let packages =
    match Github.get_file ~org ~repo ~ref ~file:"dune-project" with
    | Ok contents -> Dune_project.of_string contents
    | Error (`Msg m) -> failwith m
  in

  Package_metrics.package_install ~source ~org ~repo ~ref;

  let packages =
    if List.length packages = 0 then
      (match Github.get_file ~org ~repo ~ref ~file:(package_name ^ ".opam") with
      | Ok contents -> Opam_file.of_string ~name:package_name contents
      | Error (`Msg m) -> failwith m)
      @ packages
    else packages
  in

  packages
  |> List.map (fun (p : Dune_project.package) ->
         Package_search_index.
           {
             id = Printf.sprintf "%s/%s/%s/%s/%s" source org repo ref p.name;
             source;
             org;
             repo;
             ref;
             name = p.name;
             synopsis = p.synopsis;
             description = p.description;
             tags = p.tags;
             downloads = 0L;
           })
  |> List.iter (fun (p : Package_search_index.document) ->
         match Package_search_index.add_package p with
         | Ok _ -> ()
         | Error (`Msg msg) ->
             info (fun f ->
                 f
                   "Failed to add package %s from %s/%s/%s/%s to the search \
                    index: %s"
                   p.name source org repo ref msg));

  send reply (PackageAdded msg_ref)

let init () =
  info (fun f -> f "Initialitizing package registry");
  loop ()

let start_link () =
  let pid = spawn_link init in
  register registry_name pid;
  Ok pid

let add_package req =
  let this = self () in
  let ref = Ref.make () in
  send_by_name ~name:registry_name (AddPackage (req, ref, this));

  let selector msg =
    match msg with
    | PackageAdded ref' when Ref.equal ref ref' -> `select ()
    | _ -> `skip
  in
  receive ~selector ()
