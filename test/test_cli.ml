type command_result = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let vitrine_exe =
  match Sys.getenv_opt "VITRINE_EXE" with
  | Some value ->
      if Filename.is_relative value then Filename.concat (Sys.getcwd ()) value
      else value
  | None -> Alcotest.fail "VITRINE_EXE is not set"

let rec contains_from text needle i =
  let text_len = String.length text in
  let needle_len = String.length needle in
  i + needle_len <= text_len
  && (String.equal (String.sub text i needle_len) needle
     || contains_from text needle (i + 1))

let contains text needle = String.equal needle "" || contains_from text needle 0

let read_all channel =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buffer channel 4096
     done
   with End_of_file -> ());
  Buffer.contents buffer

let run ?cwd args =
  let original_cwd = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir original_cwd)
    (fun () ->
      Option.iter Sys.chdir cwd;
      let stdout, stdin, stderr =
        Unix.open_process_args_full vitrine_exe
          (Array.of_list (vitrine_exe :: args))
          (Unix.environment ())
      in
      close_out stdin;
      let stdout_text = read_all stdout in
      let stderr_text = read_all stderr in
      let status = Unix.close_process_full (stdout, stdin, stderr) in
      { status; stdout = stdout_text; stderr = stderr_text })

let check_success label result =
  match result.status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code ->
      Alcotest.failf "%s exited %d\nstdout:\n%s\nstderr:\n%s" label code
        result.stdout result.stderr
  | Unix.WSIGNALED signal ->
      Alcotest.failf "%s signaled %d\nstdout:\n%s\nstderr:\n%s" label signal
        result.stdout result.stderr
  | Unix.WSTOPPED signal ->
      Alcotest.failf "%s stopped %d\nstdout:\n%s\nstderr:\n%s" label signal
        result.stdout result.stderr

let check_failure label result =
  match result.status with
  | Unix.WEXITED 0 ->
      Alcotest.failf "%s unexpectedly succeeded\nstdout:\n%s\nstderr:\n%s" label
        result.stdout result.stderr
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ()

let check_contains label needle text =
  Alcotest.(check bool) label true (contains text needle)

let normalize_newlines text =
  text |> String.split_on_char '\r' |> String.concat ""

let rec find_repo_root dir =
  let site = Filename.concat dir "examples/basic-site/site" in
  if Sys.file_exists site then dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then
      Alcotest.fail "could not find repository root"
    else find_repo_root parent

let repo_root = find_repo_root (Sys.getcwd ())
let repo_path path = List.fold_left Filename.concat repo_root path

let is_directory path =
  try (Unix.stat path).st_kind = Unix.S_DIR with Unix.Unix_error _ -> false

let rec remove_tree path =
  if Sys.file_exists path then
    if is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path

let temp_counter = ref 0
let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o755

let rec mkdir_p path =
  if not (Sys.file_exists path) then (
    mkdir_p (Filename.dirname path);
    mkdir path)

let with_temp_dir name f =
  incr temp_counter;
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "vitrine-%d-%d-%s" (Unix.getpid ()) !temp_counter name)
  in
  remove_tree dir;
  mkdir_p dir;
  Fun.protect ~finally:(fun () -> remove_tree dir) (fun () -> f dir)

let write_file path content =
  mkdir_p (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel content)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let len = in_channel_length channel in
      really_input_string channel len)

let site_with files root =
  let site = Filename.concat root "site" in
  mkdir_p site;
  List.iter
    (fun (path, content) -> write_file (Filename.concat site path) content)
    files;
  site

let manifest site = run [ "manifest"; site ]
let embed site = run [ "embed"; site ]

let help_output_succeeds () =
  let result = run [ "--help=plain" ] in
  check_success "help" result;
  check_contains "help command name" "vitrine" result.stdout;
  check_contains "help command list" "manifest" result.stdout

let example_manifest_succeeds () =
  let result = manifest (repo_path [ "examples"; "basic-site"; "site" ]) in
  check_success "example manifest" result;
  check_contains "index listed" "/index.html\t" result.stdout;
  check_contains "hashed asset listed" "/asset.0123456789abcdef.txt\t"
    result.stdout

let example_embed_succeeds () =
  let result = embed (repo_path [ "examples"; "basic-site"; "site" ]) in
  check_success "example embed" result;
  check_contains "store binding" "let store =" result.stdout;
  check_contains "entry path" "Vitrine.path = \"/index.html\"" result.stdout

let missing_site_directory () =
  with_temp_dir "missing-site" @@ fun root ->
  let result = manifest (Filename.concat root "missing") in
  check_failure "missing site" result;
  check_contains "missing site error" "SITE argument" result.stderr;
  check_contains "missing site directory" "directory" result.stderr

let init_refuses_existing_target () =
  with_temp_dir "existing-init" @@ fun root ->
  let target = Filename.concat root "project" in
  mkdir_p target;
  write_file (Filename.concat target "README.md") "existing\n";
  let result = run [ "init"; target ] in
  check_failure "init existing directory" result;
  check_contains "already exists" "already exists" result.stderr

let init_generates_expected_files () =
  with_temp_dir "init-files" @@ fun root ->
  let project = Filename.concat root "demo_site" in
  let result = run [ "init"; project ] in
  check_success "init" result;
  List.iter
    (fun path ->
      Alcotest.(check bool)
        path true
        (Sys.file_exists (Filename.concat project path)))
    [
      "dune-project";
      "demo_site.opam";
      "dune";
      "config.ml";
      "unikernel.ml";
      "site/index.html";
      "site/404.html";
      "site/style.css";
      "site/app.js";
    ];
  check_contains "config package" "vitrine.mirage"
    (read_file (Filename.concat project "config.ml"));
  check_contains "site store" "Site_store.store"
    (read_file (Filename.concat project "unikernel.ml"))

let fixture_manifest_and_embed_are_deterministic () =
  with_temp_dir "deterministic" @@ fun root ->
  let site =
    site_with
      [
        ("index.html", "<h1>home</h1>\n");
        ("assets/app.js", "console.log('ok');\n");
        (".well-known/security.txt", "contact: mailto:test@example.com\n");
        ("image.bin", "\000\001PNG");
      ]
      root
  in
  let first_manifest = manifest site in
  let second_manifest = manifest site in
  let first_embed = embed site in
  let second_embed = embed site in
  check_success "first manifest" first_manifest;
  check_success "second manifest" second_manifest;
  check_success "first embed" first_embed;
  check_success "second embed" second_embed;
  Alcotest.(check string)
    "manifest output" first_manifest.stdout second_manifest.stdout;
  Alcotest.(check string) "embed output" first_embed.stdout second_embed.stdout;
  check_contains "nested file" "/assets/app.js\t" first_manifest.stdout;
  check_contains "dotfile" "/.well-known/security.txt\t" first_manifest.stdout;
  check_contains "binary content" "Vitrine.content = \"\\000\\001PNG\""
    first_embed.stdout

let empty_site_manifest_is_empty () =
  with_temp_dir "empty-site" @@ fun root ->
  let site = site_with [] root in
  let result = manifest site in
  check_success "empty site" result;
  Alcotest.(check string)
    "empty manifest" "\n"
    (normalize_newlines result.stdout)

let tests =
  [
    ("help output", `Quick, help_output_succeeds);
    ("example manifest", `Quick, example_manifest_succeeds);
    ("example embed", `Quick, example_embed_succeeds);
    ("missing site directory", `Quick, missing_site_directory);
    ("init existing target", `Quick, init_refuses_existing_target);
    ("init generated files", `Quick, init_generates_expected_files);
    ( "fixture output deterministic",
      `Quick,
      fixture_manifest_and_embed_are_deterministic );
    ("empty site manifest", `Quick, empty_site_manifest_is_empty);
  ]

let () = Alcotest.run "vitrine-cli" [ ("cli", tests) ]
