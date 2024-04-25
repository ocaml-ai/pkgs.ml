let package_download_url (package_info : Package_registry.add_package_request) =
  Package_registry.(
    Printf.sprintf {|https://pkgs.ml/p/%s/%s/%s/%s/%s|} package_info.source
      package_info.org package_info.repo package_info.ref
      package_info.package_name)
