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
  Typesense.{ api_key = env "TYPESENSE_API_KEY"; url = env "TYPESENSE_API_URL" }

let clickhouse_config =
  Clickhouse.
    {
      username = env ~default:"default" "CLICKHOUSE_USERNAME";
      password = env "CLICKHOUSE_PASSWORD";
      url =
        env ~default:"https://moj722onhz.eu-central-1.aws.clickhouse.cloud:8443"
          "CLICKHOUSE_URL";
    }
