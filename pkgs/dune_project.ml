open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "dune_project" ]
end)

exception Invalid_dune_project_file

type package = {
  name : string;
  synopsis : string;
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
      | Sexp.List (Sexp.Atom "package" :: sexp) -> Some sexp
      | _ -> None)
    sexps

let of_string raw =
  info (fun f -> f "sexpr %S" raw);
  let packages = find_package_stanzas raw in
  info (fun f -> f "packages %d" (List.length packages));
  List.map
    (fun package ->
      let name = ref None in
      let synopsis = ref None in
      let description = ref None in
      let tags = ref [] in

      List.iter
        (fun parts ->
          let open Sexplib in
          match parts with
          | Sexp.List [ Sexp.Atom "name"; Sexp.Atom v ] -> name := Some v
          | Sexp.List [ Sexp.Atom "tags"; Sexp.List v ] ->
              let v =
                List.map
                  (fun tag ->
                    match tag with
                    | Sexp.Atom tag -> tag
                    | _ -> failwith "tag was not an atom")
                  v
              in
              tags := v
          | Sexp.List [ Sexp.Atom "description"; Sexp.Atom v ] ->
              description := Some v
          | Sexp.List [ Sexp.Atom "synopsis"; Sexp.Atom v ] ->
              synopsis := Some v
          | _ -> ())
        package;

      let package =
        {
          name = !name |> Option.get;
          synopsis = !synopsis |> Option.get;
          description = !description |> Option.get;
          tags = !tags;
        }
      in

      info (fun f -> f "found package: %S" package.name);

      package)
    packages
