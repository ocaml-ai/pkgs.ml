open Ppx_yojson_conv_lib.Yojson_conv

type document = {
  id : string;
  source : string;
  org : string;
  repo : string;
  ref : string;
  pkg : string;
  synopsis : string;
  description : string;
  tags : string list;
  downloads : int64;
}
[@@deriving yojson]

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
         create_field "synopsis" String;
         create_field "description" String;
         create_field "tags" StringArray ~facet:true;
         create_field "downloads" Int64;
       ]
       ~default_sorting_field:"downloads") *)

(* {"name":"packages","fields":[{"name":"id", "type":"string"},{"name":"source","type":"string"},
   {"name":"org","type":"string"},{"name":"repo","type":"string"},{"name":"ref","type":"string"},
   {"name":"pkg","type":"string"},{"name":"synopsis","type":"string"},{"name":"description","type":"string"},{"name":"tags","type":"string[]","facet":true},
   {"name":"downloads","type":"int64"}],"default_sorting_field":"downloads"} *)

(*
curl "https://typesense.pkgs.ml/collections" \
       -X POST \
       -H "Content-Type: application/json" \
       -H "X-TYPESENSE-API-KEY: ${TYPESENSE_API_KEY}" \
       -d '{"name":"packages","fields":[{"name":"id", "type":"string"},{"name":"source","type":"string"},
   {"name":"org","type":"string"},{"name":"repo","type":"string"},{"name":"ref","type":"string"},
   {"name":"pkg","type":"string"},{"name":"synopsis","type":"string"},{"name":"description","type":"string"},{"name":"tags","type":"string[]","facet":true},
   {"name":"downloads","type":"int64"}],"default_sorting_field":"downloads"}'
   
*)

let ( let* ) = Result.bind

let run_request r =
  print_endline @@ Typesense.RequestDescriptor.show_request r;
  print_endline Config.typesense_config.url;
  Typesense_blink.make_blink_request r

(*
   let delete_collection () =
     run_request
       (Typesense.Collection.delete ~config:Config.typesense_config "packages")

   let create_collection () =
     run_request
       (Typesense.Collection.create ~config:Config.typesense_config schema) *)

let add_package document =
  run_request
    (Typesense.Document.add ~config:Config.typesense_config
       ~action:Typesense.Document.DocumentWriteParameters.Upsert
       ~collection_name
       (document |> yojson_of_document))

let search ?(p = "1") ~q () =
  let* response =
    run_request
      (Typesense.Search.search ~config:Config.typesense_config ~collection_name
         ~search_params:
           (Typesense.Search.make_search_params ~q ~query_by:"org,pkg"
              ~query_by_weights:"1,3" ~facet_by:"tags" ~per_page:100
              ~page:(p |> int_of_string) ())
         ())
  in
  let r =
    try
      response |> Yojson.Safe.from_string
      |> Typesense.Search.SearchResponse.t_of_yojson
    with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (s, _) ->
      failwith (Printexc.to_string s)
  in
  let result =
    ( List.map
        (fun (hit : Typesense.Search.SearchResponse.search_response_hit) ->
          try hit.document |> document_of_yojson
          with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (s, _) ->
            failwith (Printexc.to_string s))
        r.hits,
      r.found,
      r.page,
      r.facet_counts )
  in
  Ok result
