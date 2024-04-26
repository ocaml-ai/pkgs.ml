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

    let* conn = Blink.connect (Uri.of_string host) in

    let body = body |> Option.map Riot.Bytestring.of_string in

    let req = Http.Request.make ~meth ~headers path in

    let* conn = Blink.request ?body conn req () in
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
          | `Data data -> Some (Riot.Bytestring.to_string data)
          | _ -> None)
        parts
      |> Option.to_result ~none:`no_data_in_file
    in
    Ok data

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
