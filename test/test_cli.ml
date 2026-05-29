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

let check_not_contains label needle text =
  Alcotest.(check bool) label false (contains text needle)

let normalize_newlines text =
  text |> String.split_on_char '\r' |> String.concat ""

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

let missing_site_directory () =
  with_temp_dir "missing-site" @@ fun root ->
  let result = manifest (Filename.concat root "missing") in
  check_failure "missing site" result;
  check_contains "missing site error" "SITE argument" result.stderr;
  check_contains "missing site directory" "directory" result.stderr

let empty_site_directory () =
  with_temp_dir "empty-site" @@ fun root ->
  let site = site_with [] root in
  let result = manifest site in
  check_success "empty site" result;
  Alcotest.(check string)
    "empty manifest" "\n"
    (normalize_newlines result.stdout)

let index_html_present () =
  with_temp_dir "index-present" @@ fun root ->
  let site = site_with [ ("index.html", "<h1>ok</h1>\n") ] root in
  let result = manifest site in
  check_success "index present" result;
  check_contains "index listed" "/index.html\t" result.stdout

let index_html_missing () =
  with_temp_dir "index-missing" @@ fun root ->
  let site = site_with [ ("style.css", "body {}\n") ] root in
  let result = manifest site in
  check_success "index missing" result;
  check_contains "style listed" "/style.css\t" result.stdout;
  check_not_contains "index absent" "/index.html\t" result.stdout

let nested_and_dotfiles_discovered () =
  with_temp_dir "nested-dotfiles" @@ fun root ->
  let site =
    site_with
      [
        ("assets/app.js", "console.log('ok');\n");
        (".well-known/security.txt", "contact: mailto:test@example.com\n");
        (".env", "VISIBLE=1\n");
      ]
      root
  in
  let result = manifest site in
  check_success "nested and dotfiles" result;
  check_contains "nested file" "/assets/app.js\t" result.stdout;
  check_contains "dot directory file" "/.well-known/security.txt\t"
    result.stdout;
  check_contains "dotfile" "/.env\t" result.stdout

let binary_asset_embedded () =
  with_temp_dir "binary-asset" @@ fun root ->
  let site = site_with [ ("image.bin", "\000\001PNG") ] root in
  let result = embed site in
  check_success "embed binary" result;
  check_contains "binary path" "Vitrine.path = \"/image.bin\"" result.stdout;
  check_contains "binary content" "Vitrine.content = \"\\000\\001PNG\""
    result.stdout

let unknown_flag_exits_nonzero () =
  let result = run [ "--definitely-unknown" ] in
  check_failure "unknown flag" result

let help_output_succeeds () =
  let result = run [ "--help=plain" ] in
  check_success "help" result;
  check_contains "help command name" "vitrine" result.stdout;
  check_contains "help command list" "manifest" result.stdout

let output_directory_already_exists () =
  with_temp_dir "existing-init" @@ fun root ->
  let target = Filename.concat root "project" in
  mkdir_p target;
  let result = run [ "init"; target ] in
  check_failure "init existing directory" result;
  check_contains "already exists" "already exists" result.stderr

let output_directory_creation_failure () =
  with_temp_dir "init-parent-file" @@ fun root ->
  let parent = Filename.concat root "parent" in
  write_file parent "not a directory\n";
  let result = run [ "init"; Filename.concat parent "child" ] in
  check_failure "init under file" result;
  check_contains "not a directory" "exists and is not a directory" result.stderr

let relative_path_input () =
  with_temp_dir "relative-path" @@ fun root ->
  let _site = site_with [ ("index.html", "<h1>relative</h1>\n") ] root in
  let result = run ~cwd:root [ "manifest"; "site" ] in
  check_success "relative manifest" result;
  check_contains "relative index" "/index.html\t" result.stdout

let absolute_path_input () =
  with_temp_dir "absolute-path" @@ fun root ->
  let site = site_with [ ("index.html", "<h1>absolute</h1>\n") ] root in
  let result = manifest site in
  check_success "absolute manifest" result;
  check_contains "absolute index" "/index.html\t" result.stdout

let generated_mirage_files_reference_vitrine () =
  with_temp_dir "init-files" @@ fun root ->
  let project = Filename.concat root "demo_site" in
  let result = run [ "init"; project ] in
  check_success "init" result;
  let config = read_file (Filename.concat project "config.ml") in
  let dune = read_file (Filename.concat project "dune") in
  let unikernel = read_file (Filename.concat project "unikernel.ml") in
  check_contains "config package"
    "package ~libs:[ \"vitrine\"; \"vitrine.mirage\" ] \"vitrine\"" config;
  check_contains "dune libraries" "vitrine vitrine.mirage cohttp cohttp-lwt lwt"
    dune;
  check_contains "unikernel callback" "Vitrine_mirage.callback" unikernel;
  check_contains "site store" "Site_store.store" unikernel

let generated_output_is_deterministic () =
  with_temp_dir "deterministic" @@ fun root ->
  let site =
    site_with
      [
        ("index.html", "<h1>home</h1>\n");
        ("assets/app.js", "console.log('ok');\n");
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
  Alcotest.(check string) "embed output" first_embed.stdout second_embed.stdout

let tests =
  [
    ("missing site directory", `Quick, missing_site_directory);
    ("empty site directory", `Quick, empty_site_directory);
    ("index present", `Quick, index_html_present);
    ("index missing", `Quick, index_html_missing);
    ("nested and dotfiles", `Quick, nested_and_dotfiles_discovered);
    ("binary asset embedded", `Quick, binary_asset_embedded);
    ("unknown flag", `Quick, unknown_flag_exits_nonzero);
    ("help output", `Quick, help_output_succeeds);
    ("existing output directory", `Quick, output_directory_already_exists);
    ("output creation failure", `Quick, output_directory_creation_failure);
    ("relative path input", `Quick, relative_path_input);
    ("absolute path input", `Quick, absolute_path_input);
    ( "generated Mirage files reference Vitrine",
      `Quick,
      generated_mirage_files_reference_vitrine );
    ( "generated output is deterministic",
      `Quick,
      generated_output_is_deterministic );
  ]

let () = Alcotest.run "vitrine-cli" [ ("cli", tests) ]
