module Request = struct
  open Cohttp
  open Cohttp_lwt_unix
  open Lwt.Infix

  let make ~meth ~headers ~params ~body ~host path =
    let uri =
      Uri.of_string (host ^ path) |> fun uri -> Uri.add_query_params uri params
    in
    let headers = Header.of_list headers in

    let body = match body with None -> `Empty | Some b -> `String b in

    Client.call meth ~headers ~body uri >>= fun (resp, body) ->
    let status = Response.status resp in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    match Code.code_of_status status with
    | 200 | 201 -> Ok (`Success body)
    | _ ->
        Error
          (`Msg
            (Printf.sprintf "HTTP error %d\nbody: %s"
               (Code.code_of_status status)
               body))

  let get ~headers ~params ~host path =
    make ~meth:`GET ~headers ~params ~body:None ~host path

  let post ~headers ~params ~host ~body path =
    make ~meth:`POST ~headers ~params ~body:(Some body) ~host path

  let delete ~headers ~params ~host path =
    make ~meth:`DELETE ~headers ~params ~body:None ~host path

  let patch ~headers ~params ~host ~body path =
    make ~meth:`PATCH ~headers ~params ~body:(Some body) ~host path

  let put ~headers ~params ~host ~body path =
    make ~meth:`PUT ~headers ~params ~body:(Some body) ~host path
end

let make_cohttp_lwt_request r =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* response =
       match r with
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
     in
     match response with
     | Ok (`Success data) -> Lwt.return (Ok data)
     | Error e -> Lwt.return (Error e))
