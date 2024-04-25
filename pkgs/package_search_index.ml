let collection_name = "packages"

(* let schema =
   Typesense.Schema.(
     schema collection_name
       [
         create_field "source" String;
         create_field "org" String;
         create_field "repo" String;
         create_field "ref" String;
         create_field "pkg" String;
         create_field "tags" StringArray ~facet:true;
         create_field "downloads" Int64;
       ]
       ~default_sorting_field:"downloads") *)

(* {"name":"packages","fields":[{"name":"id", "type":"string"},{"name":"source","type":"string"},{"name":"org","type":"string"},{"name":"repo","type":"string"},{"name":"ref","type":"string"},{"name":"pkg","type":"string"},{"name":"tags","type":"string[]","facet":true},{"name":"downloads","type":"int64"}],"default_sorting_field":"downloads"} *)

let run_request r =
  print_endline @@ Typesense.RequestDescriptor.show_request r;
  print_endline Config.typesense_config.url;
  let response = Typesense_blink.make_blink_request r in
  match response with
  | Ok (`Success response) -> print_endline response
  | Error (`Msg m) -> print_endline m

(*
   let delete_collection () =
     run_request
       (Typesense.Collection.delete ~config:Config.typesense_config "packages")

   let create_collection () =
     run_request
       (Typesense.Collection.create ~config:Config.typesense_config schema) *)

open Ppx_yojson_conv_lib.Yojson_conv

type document = {
  id : string;
  source : string;
  org : string;
  repo : string;
  ref : string;
  pkg : string;
  tags : string list;
  downloads : int64;
}
[@@deriving yojson]

let add_package document =
  run_request
    (Typesense.Document.add ~config:Config.typesense_config
       ~action:Typesense.Document.DocumentWriteParameters.Upsert
       ~collection_name
       (document |> yojson_of_document))
