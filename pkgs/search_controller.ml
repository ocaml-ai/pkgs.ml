open Riot

open Logger.Make (struct
  let namespace = [ "pkgs"; "search_controller" ]
end)

open Trail

let get conn =
  let q = Trail.Conn.(conn.req.query) |> List.assoc "q" |> List.hd in
  let p =
    try Trail.Conn.(conn.req.query) |> List.assoc "p" |> List.hd
    with Not_found -> "1"
  in
  let packages, found, _, facet_counts =
    Package_search_index.search
      ~search_params:
        (Typesense.Search.make_search_params ~q ~query_by:"org,name,synopsis,description,tags"
           ~query_by_weights:"3,8,1,1,1" ~facet_by:"tags" ~per_page:100
           ~page:(p |> int_of_string) ())
      ()
    |> Result.get_ok
  in
  let html = Template_search.render ~found ~q ~packages ~facet_counts |> Html_of_jsx.render in
  conn |> Conn.send_response `OK {%b|"<!doctype html>"::string,html::string|}
