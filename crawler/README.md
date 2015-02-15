# Crawler

## Dependencies

- [Lwt](http://ocsigen.org/lwt/)
- cryptokit
- [PG'OCaml](http://pgocaml.forge.ocamlcore.org/)
- [Yojson](http://mjambon.com/yojson.html)

You can install them with [OPAM](http://opam.ocaml.org/):

    opam install lwt cryptokit pgocaml yojson

You also need a running [PostgreSQL](http://www.postgresql.org/) server and environment variable `$PGDATABASE` set to `pantor` (and if the database is not local, connection information should be stored in `$PGUSER`, `$PGPASSWORD`, `$PGHOST` and `$PGPORT`).
The database server must be available at compile time and at run time.

## Build

    ocamlbuild -use-ocamlfind main.native

## Run

Assuming your ip is `1.2.3.4` and you want to use ports 1337 to 1337+511:

    ./main.native 1.2.3.4 1337

You can run several instances at the same time (just make sure their ports ranges don't overlap if they are on the same machine).
