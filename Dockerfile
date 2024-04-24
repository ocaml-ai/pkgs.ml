# syntax=docker/dockerfile:1
# FROM ocaml/opam:ubuntu-22.04-ocaml-5.1
FROM ocaml/opam:alpine-ocaml-5.1-flambda
WORKDIR /app

USER 0

# PINNED DEPS
# weird dep that we need because crypto stuff needs zarith
# RUN apt-get update -y && apt-get install libgmp-dev -y
RUN apk add gmp-dev

# hack to get opam 2.1 running
RUN ln -f /usr/bin/opam-2.1 /usr/bin/opam
RUN opam --version
RUN opam init

RUN echo "cache-version: 0"

# ACTUAL APP DEPS
COPY *.opam .
RUN opam install --deps-only --with-test ./pkgs.opam

# BUILD APP
COPY . .
RUN eval $(opam env) && dune build --profile=docker @all

FROM scratch
COPY --from=0 /app/_build/default/pkgs/pkgs.exe /bin/pkgs
CMD ["/bin/pkgs"]
