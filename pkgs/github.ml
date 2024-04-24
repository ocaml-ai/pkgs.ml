open Riot
let ( let* ) = Result.bind

let github_user_content_host =
  "https://raw.githubusercontent.com" |> Uri.of_string

let get_ref_tarball ~org ~repo ~ref = 
  Format.sprintf "https://github.com/%s/%s/archive/%s.tar.gz" org repo ref

let get_file ~org ~repo ~ref ~file =
  let* conn = Blink.connect github_user_content_host in
  let req =
    let url = Format.sprintf "%s/%s/%s/%s" org repo ref file in
    Http.Request.make url
  in
  let* conn = Blink.request conn req () in
  let rec stream conn acc =
    let* conn, frames = Blink.stream conn in
    match frames with
    | [ `Done ] -> Ok (List.rev acc)
    | frames -> stream conn (frames @ acc)
  in
  let* parts = stream conn [] in
  let* data =
    List.find_map
      (fun (frame : Blink.Connection.message) ->
        match frame with
        | `Data data -> Some (Bytestring.to_string data)
        | _ -> None)
      parts
    |> Option.to_result ~none:`no_data_in_file
  in
  Ok data
