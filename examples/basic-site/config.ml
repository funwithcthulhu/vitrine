open Mirage

let main =
  main
    ~packages:[ package ~libs:[ "vitrine"; "vitrine.mirage" ] "vitrine" ]
    "Unikernel.Make"
    (http @-> job)

let stack = generic_stackv4v6 default_network
let http = cohttp_server (conduit_direct ~tls:false stack)

let () = register "vitrine_basic_site" [ main $ http ]
