open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "clickhouse" ]
end)

let ( let* ) = Result.bind

type config = { username : string; password : string; url : string }

let execute config query =
  let* conn = Blink.connect (Uri.of_string config.url) in
  let req =
    let headers =
      let token = Base64.encode_exn (config.username ^ ":" ^ config.password) in
      Http.Header.of_list [ ("authorization", "Basic " ^ token) ]
    in
    Http.Request.make ~meth:`POST ~headers "/"
  in
  let* conn =
    info (fun f -> f "sending query: %S" query);
    let body = Bytestring.of_string query in
    Blink.request conn req ~body ()
  in
  let rec stream conn acc =
    let* conn, frames = Blink.stream conn in
    match frames with
    | [ `Done ] -> Ok (List.rev acc)
    | frames -> stream conn (frames @ acc)
  in
  let* parts = stream conn [] in
  List.iter
    (fun (frame : Blink.Connection.message) ->
      match frame with
      | `Headers headers ->
          let headers = Http.Header.to_string headers in
          info (fun f -> f "headers: %S" headers)
      | `Status status ->
          info (fun f -> f "status: %S" (Http.Status.to_string status))
      | _ -> ())
    parts;
  let* data =
    List.find_map
      (fun (frame : Blink.Connection.message) ->
        match frame with
        | `Data data -> Some (Bytestring.to_string data)
        | _ -> None)
      parts
    |> Option.to_result ~none:`no_data_in_response
  in
  info (fun f -> f "data: %S" data);
  Ok data
