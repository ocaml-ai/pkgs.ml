open Riot
open Trail

type publish_request = {
  source : string;
  org : string;
  repo : string;
  version : string;
  name : string;
}
[@@deriving deserialize]

let[@warning "-8"] publish conn =
  let Package_registry.({ ref = _; _ } as req) =
    Trail.Conn.
      {
        source = conn.params |> List.assoc "source";
        org = conn.params |> List.assoc "org";
        repo = conn.params |> List.assoc "repo";
        ref = conn.params |> List.assoc "ref";
        package_name = conn.params |> List.assoc "pkg";
      }
  in

  let _ = spawn (fun () -> Package_registry.add_package req) in

  (* let (Conn.Ok (conn, body)) = Conn.read_body conn in
     let (Ok req) =
       Serde_json.of_string deserialize_publish_request (Bytestring.to_string body)
     in

     Package_registry.add_package
       {
         source = req.source;
         org = req.source;
         repo = req.repo;
         ref = req.version;
         package_name = req.name;
       }; *)
  conn |> Conn.send_status `OK
