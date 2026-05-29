open Cmdliner

let fail message = Error message

let is_directory path =
  try (Unix.stat path).st_kind = Unix.S_DIR with Unix.Unix_error _ -> false

let mkdir path =
  if Sys.file_exists path then
    if is_directory path then Ok ()
    else fail (path ^ " exists and is not a directory")
  else
    try
      Unix.mkdir path 0o755;
      Ok ()
    with Unix.Unix_error (error, _, _) ->
      fail (path ^ ": " ^ Unix.error_message error)

let rec mkdir_p path =
  if String.equal path "." || String.equal path "" then Ok ()
  else if Sys.file_exists path then
    if is_directory path then Ok ()
    else fail (path ^ " exists and is not a directory")
  else
    match mkdir_p (Filename.dirname path) with
    | Error _ as error -> error
    | Ok () -> mkdir path

let write_file path content =
  match mkdir_p (Filename.dirname path) with
  | Error _ as error -> error
  | Ok () -> (
      try
        let channel = open_out_bin path in
        output_string channel content;
        close_out channel;
        Ok ()
      with Sys_error message -> fail message)

let read_file path =
  try
    let channel = open_in_bin path in
    let len = in_channel_length channel in
    let content = really_input_string channel len in
    close_in channel;
    Ok content
  with Sys_error message -> fail message

let http_date time =
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let months =
    [|
      "Jan";
      "Feb";
      "Mar";
      "Apr";
      "May";
      "Jun";
      "Jul";
      "Aug";
      "Sep";
      "Oct";
      "Nov";
      "Dec";
    |]
  in
  let tm = Unix.gmtime time in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" days.(tm.Unix.tm_wday)
    tm.Unix.tm_mday months.(tm.Unix.tm_mon) (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let entry_path parts = "/" ^ String.concat "/" parts

let rec scan_files root parts =
  let directory = List.fold_left Filename.concat root parts in
  let names =
    Sys.readdir directory |> Array.to_list |> List.sort String.compare
  in
  List.fold_left
    (fun result name ->
      match result with
      | Error _ as error -> error
      | Ok acc -> (
          let path = Filename.concat directory name in
          let parts = parts @ [ name ] in
          if is_directory path then
            match scan_files root parts with
            | Error _ as error -> error
            | Ok children -> Ok (acc @ children)
          else
            match read_file path with
            | Error _ as error -> error
            | Ok content ->
                let last_modified =
                  try Some (http_date (Unix.stat path).Unix.st_mtime)
                  with Unix.Unix_error _ -> None
                in
                let entry =
                  {
                    Vitrine.path = entry_path parts;
                    file = { content; last_modified };
                  }
                in
                Ok (acc @ [ entry ])))
    (Ok []) names

let load_site root =
  if not (Sys.file_exists root) then fail (root ^ " does not exist")
  else if not (is_directory root) then fail (root ^ " is not a directory")
  else scan_files root []

let cache_class_to_string = function
  | Vitrine.Html -> "html"
  | Vitrine.Immutable -> "immutable"
  | Vitrine.Static -> "static"

let run_or_exit result =
  match result with
  | Ok () -> ()
  | Error message ->
      prerr_endline ("vitrine: " ^ message);
      exit 1

let manifest output site =
  let result =
    match load_site site with
    | Error _ as error -> error
    | Ok entries -> (
        let store =
          entries |> Vitrine.Memory_store.of_entries
          |> Vitrine.Memory_store.store
        in
        let lines =
          Vitrine.manifest store
          |> List.map (fun entry ->
              Printf.sprintf "%s\t%d\t%s\t%s\t%s\t%s"
                entry.Vitrine.manifest_path entry.size entry.sha256
                entry.manifest_mime_type
                (cache_class_to_string entry.manifest_cache_class)
                entry.manifest_cache_control)
        in
        let content = String.concat "\n" lines ^ "\n" in
        match output with
        | None ->
            print_string content;
            Ok ()
        | Some path -> write_file path content)
  in
  run_or_exit result

let ocaml_option_string = function
  | None -> "None"
  | Some value -> "Some " ^ Printf.sprintf "%S" value

let embed site =
  let result =
    match load_site site with
    | Error _ as error -> error
    | Ok entries ->
        let entries =
          List.sort
            (fun (a : Vitrine.entry) (b : Vitrine.entry) ->
              String.compare a.path b.path)
            entries
        in
        print_endline "let store =";
        print_endline "  Vitrine.Memory_store.of_entries";
        print_endline "    [";
        List.iter
          (fun (entry : Vitrine.entry) ->
            Printf.printf "      {\n";
            Printf.printf "        Vitrine.path = %S;\n" entry.path;
            Printf.printf "        file =\n";
            Printf.printf "          {\n";
            Printf.printf "            Vitrine.content = %S;\n"
              entry.file.content;
            Printf.printf "            last_modified = %s;\n"
              (ocaml_option_string entry.file.last_modified);
            Printf.printf "          };\n";
            Printf.printf "      };\n")
          entries;
        print_endline "    ]";
        print_endline "  |> Vitrine.Memory_store.store";
        Ok ()
  in
  run_or_exit result

let init_dune_project name = Printf.sprintf {|(lang dune 3.8)

(name %s)
|} name

let init_opam _name =
  {|opam-version: "2.0"
synopsis: "Static MirageOS site built with Vitrine"
maintainer: "funwithcthulhu29@gmail.com"
depends: [
  "ocaml" {>= "5.1"}
  "dune" {>= "3.8"}
  "vitrine"
  "cohttp"
  "cohttp-lwt"
  "lwt"
]
build: [
  ["dune" "build"]
]
|}

let init_root_dune =
  {|(executable
 (name unikernel)
 (modules unikernel site_store)
 (libraries vitrine vitrine.mirage cohttp cohttp-lwt lwt))

(rule
 (targets site_store.ml)
 (deps (source_tree site))
 (action (with-stdout-to %{targets} (run vitrine embed site))))
|}

let init_config name =
  Printf.sprintf
    {|open Mirage

let main =
  main
    ~packages:[ package ~libs:[ "vitrine"; "vitrine.mirage" ] "vitrine" ]
    "Unikernel.Make"
    (http @-> job)

let stack = generic_stackv4v6 default_network
let http = cohttp_server (conduit_direct ~tls:false stack)

let () = register %S [ main $ http ]
|}
    name

let init_unikernel =
  {|module Make (Server : Cohttp_lwt.S.Server) = struct
  let routes =
    [
      {
        Vitrine_mirage.meth = `GET;
        path = "/health";
        handler =
          (fun _request _body ->
            let response = Vitrine.text "ok\n" |> Vitrine_mirage.response in
            Lwt.return response);
      };
    ]

  let start server =
    let callback =
      Vitrine_mirage.callback ~routes Site_store.store
    in
    Server.callback (Server.make ~callback ()) server

  let _ = start
end
|}

let init_index name =
  Printf.sprintf
    {|<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="/style.css">
  <title>%s</title>
</head>
<body>
  <main>
    <h1>%s</h1>
    <p>This page is served from an embedded site directory.</p>
  </main>
  <script src="/app.js"></script>
</body>
</html>
|}
    name name

let init_404 =
  {|<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Not found</title>
</head>
<body>
  <h1>Not found</h1>
  <p>The requested page does not exist.</p>
</body>
</html>
|}

let init_css =
  {|html {
  font-family: system-ui, sans-serif;
  color: #202020;
  background: #f7f7f4;
}

body {
  margin: 0;
}

main {
  max-width: 42rem;
  margin: 4rem auto;
  padding: 0 1rem;
}
|}

let init_js = {|document.documentElement.dataset.vitrine = "ready";
|}

let init_readme name =
  Printf.sprintf
    {|# %s

Static site served by a MirageOS unikernel.

## Build

Generate the embedded store and build the Unix executable:

```sh
dune build
```

With Mirage installed:

```sh
mirage configure -t unix
make
```

For Solo5:

```sh
mirage configure -t hvt
make
```
|}
    name

let project_name path =
  path |> String.split_on_char '/'
  |> List.concat_map (String.split_on_char '\\')
  |> List.filter (fun part -> not (String.equal part ""))
  |> List.rev
  |> function
  | name :: _ -> (
      match String.split_on_char ':' name with
      | [ ""; fallback ] -> fallback
      | _ -> name)
  | [] -> path

let init name =
  let result =
    if Sys.file_exists name then fail (name ^ " already exists")
    else
      let project_name = project_name name in
      let files =
        [
          ("dune-project", init_dune_project project_name);
          (project_name ^ ".opam", init_opam project_name);
          ("dune", init_root_dune);
          ("config.ml", init_config project_name);
          ("unikernel.ml", init_unikernel);
          ("site/index.html", init_index project_name);
          ("site/404.html", init_404);
          ("site/style.css", init_css);
          ("site/app.js", init_js);
          ("README.md", init_readme project_name);
        ]
      in
      match mkdir_p name with
      | Error _ as error -> error
      | Ok () ->
          List.fold_left
            (fun result (path, content) ->
              match result with
              | Error _ as error -> error
              | Ok () -> write_file (Filename.concat name path) content)
            (Ok ()) files
  in
  run_or_exit result

let dev port site =
  let result =
    match load_site site with
    | Error _ as error -> error
    | Ok entries ->
        let store =
          entries |> Vitrine.Memory_store.of_entries
          |> Vitrine.Memory_store.store
        in
        let callback = Vitrine_mirage.callback store in
        let server = Cohttp_lwt_unix.Server.make ~callback () in
        Printf.eprintf "Serving %s on http://127.0.0.1:%d\n%!" site port;
        Lwt_main.run
          (Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port)) server);
        Ok ()
  in
  run_or_exit result

let site_arg =
  let doc = "Site directory." in
  Arg.(required & pos 0 (some dir) None & info [] ~docv:"SITE" ~doc)

let name_arg =
  let doc = "Project directory to create." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let output_arg =
  let doc = "Write output to this file." in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let port_arg =
  let doc = "Port to listen on." in
  Arg.(value & opt int 8080 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let init_cmd =
  let doc = "Create a small Vitrine project." in
  Cmd.v (Cmd.info "init" ~doc) Term.(const init $ name_arg)

let manifest_cmd =
  let doc = "Print deterministic site metadata." in
  Cmd.v (Cmd.info "manifest" ~doc) Term.(const manifest $ output_arg $ site_arg)

let embed_cmd =
  let doc = "Emit an OCaml store for a site directory." in
  Cmd.v (Cmd.info "embed" ~doc) Term.(const embed $ site_arg)

let dev_cmd =
  let doc = "Serve a site directory locally." in
  Cmd.v (Cmd.info "dev" ~doc) Term.(const dev $ port_arg $ site_arg)

let cmd =
  let doc = "Compile a site directory into a MirageOS web appliance." in
  Cmd.group
    (Cmd.info "vitrine" ~version:"0.1.0" ~doc)
    [ init_cmd; manifest_cmd; embed_cmd; dev_cmd ]

let () = exit (Cmd.eval cmd)
