module Request = struct
  [@@@warning "-8"]

  let ( let* ) = Result.bind

  let make ~meth ?(headers = []) ?(params = []) ?(body = None) ~host path =
    let headers =
      Http.Header.init () |> fun h -> Http.Header.add_list h headers
    in
    let path =
      Uri.of_string path |> fun path ->
      Uri.add_query_params path params |> Uri.to_string
    in

    let* conn =
      Blink.connect (Uri.of_string host)
      |> Result.map_error (fun e ->
             match e with
             | `Closed -> `Msg "closed on Blink.connect"
             | `Invalid_uri u -> `Msg ("invalid uri: " ^ Uri.to_string u)
             | `Msg m -> `Msg m
             | `Tls_error _ -> `Msg "Tls_error"
             | `Unix_error e -> `Msg (Unix.error_message e)
             | _ -> `Msg "other error")
    in

    let req = Http.Request.make ~meth ~headers path in
    let body = body |> Option.map Riot.Bytestring.of_string in
    let* conn =
      Blink.request ?body conn req ()
      |> Result.map_error (fun e ->
             match e with
             | `Closed -> `Msg "closed on Blink.request"
             | `Unix_error e -> `Msg (Unix.error_message e)
             | _ -> `Msg "other error")
    in
    let* _conn, [ `Status status; `Headers _headers; `Data body; `Done ] =
      Blink.stream conn
      |> Result.map_error (fun e ->
             match e with
             | `Closed -> `Msg "closed on Blink.stream"
             | `Eof -> `Msg "Eof"
             | `Response_parsing_error -> `Msg "Response_parsing_error"
             | `Unix_error e -> `Msg (Unix.error_message e)
             | _ -> `Msg "other error")
    in

    match status with
    | `OK -> Ok (`Success (Riot.Bytestring.to_string body))
    | _ ->
        Riot.Logger.error (fun f ->
            f "> Got response:\n%s\n%s\n%d\n%!"
              (Http.Status.to_string status)
              (Http.Header.to_lines headers |> String.concat "")
              (Riot.Bytestring.length body));

        Error
          (`Msg
            (Printf.sprintf "> Got response:\n%s\n%s\n%d\n%!"
               (Http.Status.to_string status)
               (Http.Header.to_lines headers |> String.concat "")
               (Riot.Bytestring.length body)))

  let get ?(headers = []) ?(params = []) ~host path =
    make ~meth:`GET ~headers ~params ~host path

  let post ?(headers = []) ?(params = []) ~host ~body path =
    make ~meth:`POST ~headers ~params ~host ~body:(Some body) path

  let delete ?(headers = []) ?(params = []) ~host path =
    make ~meth:`DELETE ~headers ~params ~host path

  let patch ?(headers = []) ?(params = []) ~host ~body path =
    make ~meth:`PATCH ~headers ~params ~host ~body:(Some body) path

  let put ?(headers = []) ?(params = []) ~host ~body path =
    make ~meth:`PUT ~headers ~params ~host ~body:(Some body) path
end

let config =
  let api_key =
    try Sys.getenv "TYPESENSE_API_KEY" with _ -> "{TYPESENSE_API_KEY}"
  in
  let url =
    try Sys.getenv "TYPESENSE_HOST" with _ -> "http://localhost:8108"
  in
  Typesense.{ api_key; url }

let make_blink_request = function
  | Typesense.RequestDescriptor.Get { host; path; headers; params } ->
      Request.get ~headers ~params ~host path
  | Post { host; path; headers; params; body } ->
      Request.post ~headers ~params ~host ~body path
  | Delete { host; path; headers; params } ->
      Request.delete ~headers ~params ~host path
  | Patch { host; path; headers; params; body } ->
      Request.patch ~headers ~params ~host ~body path
  | Put { host; path; headers; params; body } ->
      Request.put ~headers ~params ~host ~body path
