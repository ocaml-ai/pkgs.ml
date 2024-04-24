open Riot
open Trail

type request = {
  source : string;
  org : string;
  repo : string;
  ref : string;
  package_name : string;
}

let proxy conn =
  let req =
    Trail.Conn.
      {
        source = conn.params |> List.assoc "source";
        org = conn.params |> List.assoc "org";
        repo = conn.params |> List.assoc "repo";
        ref = conn.params |> List.assoc "ref";
        package_name = conn.params |> List.assoc "pkg";
      }
  in

  let redirect =
    Format.sprintf "https://%s/%s/%s/%s" req.source req.org req.repo req.ref
  in

  conn |> Conn.with_header "Location" redirect |> Conn.send_response `Temporary_redirect {%b||}
