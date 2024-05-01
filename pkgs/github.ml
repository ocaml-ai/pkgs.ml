open Riot

open Logger.Make (struct
  let namespace = [ "github" ]
end)

let ( let* ) = Result.bind

let github_user_content_host =
  "https://raw.githubusercontent.com" |> Uri.of_string

let get_repo_url ~org ~repo = Format.sprintf "https://github.com/%s/%s" org repo

let get_ref_tarball ~org ~repo ~ref =
  Format.sprintf "%s/archive/%s.tar.gz" (get_repo_url ~org ~repo) ref

let get_file ~org ~repo ~ref ~file =
  let response =
    let* conn = Blink.connect github_user_content_host in
    let req =
      let url = Format.sprintf "%s/%s/%s/%s" org repo ref file in
      info (fun f -> f "Fetching GitHub file at %s" url);
      Http.Request.make url
    in
    let* conn = Blink.request conn req () in
    let rec stream conn acc =
      let* conn, frames = Blink.stream conn in
      match frames with
      | [ `Done ] -> Ok (List.rev acc |> List.flatten)
      | frames -> stream conn (frames :: acc)
    in
    let* parts = stream conn [] in
    let* status =
      List.find_map
        (fun (frame : Blink.Connection.message) ->
          match frame with
          | `Status status -> Some (Http.Status.to_int status)
          | _ -> None)
        parts
      |> Option.to_result ~none:`no_status_found
    in
    let data =
      parts
      |> List.filter_map (fun (frame : Blink.Connection.message) ->
             match frame with
             | `Data data ->
                 info (fun f -> f "data: %S" (Bytestring.to_string data));
                 Some data
             | _ -> None)
      |> List.fold_left Bytestring.join Bytestring.empty
      |> Bytestring.to_string
    in
    Ok (status, data)
  in
  match response with
  | Ok (200, contents) -> Ok contents
  | Ok (n, _) -> Error (`Msg (Printf.sprintf "get file failed status=%d" n))
  | Error `no_data_in_file -> Error (`Msg "no data in file")
  | Error `no_status_found -> Error (`Msg "no status found")
  | Error (#Riot.IO.io_error as e) ->
      Error (`Msg (Format.asprintf "%a" Riot.IO.pp_err e))
  | _ -> Error (`Msg "other error")
