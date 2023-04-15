(lang dune 3.7)

(name ohtml)

(generate_opam_files true)
(source (github tjdevries/OhtML))
(authors "TJ DeVries")
(maintainers "TJ DeVries")
(license MIT)
; (documentation https://url/to/documentation)

(package
 (name ohtml)
 (synopsis "OhtML (Oatmeal) is a web framework where you get to have fun.")
 (description "No, seriously. The goal really is to have fun")
 (depends (ocaml (>= "5.0"))
          dune
          dream
          tyxml
          bos
          conf-libssl
          caqti-lwt caqti-driver-sqlite3
          ppx_rapper ppx_rapper_lwt))
