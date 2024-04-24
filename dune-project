(lang dune 3.13)

(name pkgs)

(generate_opam_files true)

(source
 (github ocaml-ai/pkgs))

(license MIT)

(package
 (name pkgs)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune trail nomad blink mlx)
 (tags
  (topics "to describe" your project)))

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (preprocess
   (run mlx-pp %{input-file}))))