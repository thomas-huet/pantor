# Crawler

## Dependencies

- [Lwt](http://ocsigen.org/lwt/)
- cryptokit
- [PG'OCaml](http://pgocaml.forge.ocamlcore.org/)

You can install them with [OPAM](http://opam.ocaml.org/):

`opam install lwt cryptokit pgocaml`

You also need a running [PostgreSQL](http://www.postgresql.org/) server.

## Build

`ocamlbuild -use-ocamlfind main.native`
