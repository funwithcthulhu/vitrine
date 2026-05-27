(lang dune 3.8)

(name vitrine)

(package
 (name vitrine)
 (synopsis "Compile a site directory into a MirageOS web appliance")
 (description "Static-site serving logic and a Mirage/Cohttp adapter.")
 (authors "funwithcthulhu")
 (maintainers "funwithcthulhu29@gmail.com")
 (license MIT)
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.8))
  uri
  digestif
  cmdliner
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  lwt
  (alcotest :with-test)))
