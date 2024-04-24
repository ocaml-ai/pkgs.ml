open Riot

type request = { source : string; org : string; repo : string; ref : string }

let proxy req =
  Format.sprintf "https://%s/%s/%s/%s" req.source req.org req.repo req.ref
  |>Bytestring.of_string
