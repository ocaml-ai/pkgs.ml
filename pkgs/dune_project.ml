open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "dune_project" ]
end)

exception Invalid_dune_project_file

type package = {
  name : string;
  synopsys : string;
  description : string;
  tags : string list;
}
[@@deriving deserialize, serialize]

let find_package_stanzas raw =
  let open Sexplib in
  let sexps = Sexp.of_string_many raw in
  List.filter_map
    (fun (sexp : Sexp.t) ->
      match sexp with
      | Sexp.List (Sexp.Atom "package" :: _) -> Some (Sexp.to_string_hum sexp)
      | _ -> None)
    sexps

let of_string raw =
  info (fun f -> f "sexpr %S" raw);
  let packages = find_package_stanzas raw in
  info (fun f -> f "packages %d" (List.length packages));
  List.map
    (fun package ->
      info (fun f -> f "package: %S" package);
      let package =
        Serde_sexpr.of_string deserialize_package package |> Result.get_ok
      in
      let sexp =
        Serde_json.to_string serialize_package package |> Result.get_ok
      in

      info (fun f -> f "%S" sexp);
      sexp)
    packages
