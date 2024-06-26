let package_download_url (package_info : Package_search_index.document) =
  Printf.sprintf {|https://pkgs.ml/p/%s/%s/%s/%s/%s|} package_info.source
    package_info.org package_info.repo package_info.ref package_info.name

let package_info_url (package : Package_search_index.document) =
  Printf.sprintf {|/i/%s/%s/%s/%s/%s|} package.source package.org package.repo
    package.ref package.name
