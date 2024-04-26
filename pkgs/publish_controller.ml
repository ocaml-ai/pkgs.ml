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
  let (Conn.Ok (conn, body)) = Conn.read_body conn in
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
    };

  conn |> Conn.send_status `OK
