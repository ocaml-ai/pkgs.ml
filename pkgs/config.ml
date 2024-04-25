let env ?default var =
  let exception Missing_configuration of string in
  match (Sys.getenv_opt var, default) with
  | None, None ->
      raise
        (Missing_configuration
           (Format.sprintf "Missing environment variable %s" var))
  | None, Some x -> x
  | Some x, _ -> x

let typesense_config =
  Typesense.
    {
      api_key = env "TYPESENSE_API_KEY";
      url = env "TYPESENSE_API_URL";
    }
