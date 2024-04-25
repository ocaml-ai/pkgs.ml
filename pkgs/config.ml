let typesense_config =
  Typesense.
    {
      api_key = Sys.getenv "TYPESENSE_API_KEY";
      url = Sys.getenv "TYPESENSE_API_URL";
    }
