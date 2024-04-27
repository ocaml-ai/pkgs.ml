open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "opam_file" ]
end)

exception Invalid_opam_file

let of_string ~name s =
  let opam_file =
    try Some (OpamFile.OPAM.read_from_string s) with _ -> None
  in
  match opam_file with
  | None -> []
  | Some opam_file ->
      [
        Dune_project.
          {
            name;
            synopsis =
              opam_file.descr
              |> Option.map OpamFile.Descr.synopsis
              |> Option.value ~default:"";
            description =
              opam_file.descr
              |> Option.map OpamFile.Descr.body
              |> Option.value ~default:"";
            tags = opam_file.tags;
          };
      ]
