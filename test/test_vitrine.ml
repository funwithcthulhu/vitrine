let file ?last_modified content = { Vitrine.content; last_modified }

let store entries =
  entries
  |> List.map (fun (path, content) -> { Vitrine.path; file = file content })
  |> Vitrine.Memory_store.of_entries
  |> Vitrine.Memory_store.store

let rich_store =
  store
    [
      ("/index.html", "<h1>home</h1>");
      ("/docs/index.html", "<h1>docs</h1>");
      ("/404.html", "<h1>missing</h1>");
      ("/style.css", "body{}");
      ("/app.0123456789abcdef.js", "console.log('vitrine');");
      ("/data.json", "{}");
      ("/asset.wasm", "wasm");
      ("/app.js", "plain");
      ("/app.js.gz", "gzip");
      ("/app.js.br", "brotli");
    ]

let request ?(meth = Vitrine.Get) ?(headers = []) path =
  { Vitrine.meth; path; headers }

let header response name =
  match Vitrine.header response name with
  | Some value -> value
  | None -> Alcotest.failf "missing header %s" name

let check_status expected response =
  Alcotest.(check int) "status" (Vitrine.status_to_int expected)
    (Vitrine.status_to_int response.Vitrine.status)

let resolves_root () =
  let response = Vitrine.handle rich_store (request "/") in
  check_status Vitrine.Ok response;
  Alcotest.(check string) "body" "<h1>home</h1>" response.body

let resolves_directory_index () =
  let response = Vitrine.handle rich_store (request "/docs/") in
  check_status Vitrine.Ok response;
  Alcotest.(check string) "body" "<h1>docs</h1>" response.body

let custom_404 () =
  let response = Vitrine.handle rich_store (request "/missing") in
  check_status Vitrine.Not_found response;
  Alcotest.(check string) "body" "<h1>missing</h1>" response.body

let rejects_traversal () =
  let response = Vitrine.handle rich_store (request "/../secret") in
  check_status Vitrine.Bad_request response

let rejects_encoded_traversal () =
  let response = Vitrine.handle rich_store (request "/%2e%2e/secret") in
  check_status Vitrine.Bad_request response

let mime_types () =
  let cases =
    [
      ("/index.html", "text/html; charset=utf-8");
      ("/style.css", "text/css; charset=utf-8");
      ("/app.js", "text/javascript; charset=utf-8");
      ("/module.mjs", "text/javascript; charset=utf-8");
      ("/data.json", "application/json");
      ("/feed.xml", "application/xml");
      ("/image.png", "image/png");
      ("/photo.jpg", "image/jpeg");
      ("/photo.jpeg", "image/jpeg");
      ("/image.gif", "image/gif");
      ("/icon.svg", "image/svg+xml");
      ("/image.webp", "image/webp");
      ("/favicon.ico", "image/x-icon");
      ("/asset.wasm", "application/wasm");
      ("/file.pdf", "application/pdf");
      ("/archive.zip", "application/zip");
      ("/archive.gz", "application/gzip");
      ("/font.woff", "font/woff");
      ("/font.woff2", "font/woff2");
      ("/font.ttf", "font/ttf");
      ("/font.otf", "font/otf");
    ]
  in
  List.iter
    (fun (path, expected) ->
      Alcotest.(check string) path expected (Vitrine.mime_type path))
    cases

let get_returns_body () =
  let response = Vitrine.handle rich_store (request "/style.css") in
  check_status Vitrine.Ok response;
  Alcotest.(check string) "body" "body{}" response.body

let head_returns_headers_only () =
  let response = Vitrine.handle rich_store (request ~meth:Vitrine.Head "/style.css") in
  check_status Vitrine.Ok response;
  Alcotest.(check string) "body" "" response.body;
  Alcotest.(check string) "length" "6" (header response "Content-Length")

let etag_emitted () =
  let response = Vitrine.handle rich_store (request "/style.css") in
  Alcotest.(check bool) "quoted etag"
    true
    (let etag = header response "ETag" in
     String.length etag > 2 && etag.[0] = '"')

let if_none_match_returns_304 () =
  let first = Vitrine.handle rich_store (request "/style.css") in
  let response =
    Vitrine.handle rich_store
      (request ~headers:[ ("If-None-Match", header first "ETag") ] "/style.css")
  in
  check_status Vitrine.Not_modified response;
  Alcotest.(check string) "body" "" response.body

let immutable_cache_for_hashed_assets () =
  let response = Vitrine.handle rich_store (request "/app.0123456789abcdef.js") in
  Alcotest.(check string) "cache" "public, max-age=31536000, immutable"
    (header response "Cache-Control")

let html_cache_is_revalidate_friendly () =
  let response = Vitrine.handle rich_store (request "/") in
  Alcotest.(check string) "cache" "no-cache" (header response "Cache-Control")

let brotli_preferred_over_gzip () =
  let response =
    Vitrine.handle rich_store
      (request ~headers:[ ("Accept-Encoding", "gzip, br") ] "/app.js")
  in
  check_status Vitrine.Ok response;
  Alcotest.(check string) "encoding" "br" (header response "Content-Encoding");
  Alcotest.(check string) "body" "brotli" response.body

let gzip_used_when_brotli_unavailable () =
  let only_gzip = store [ ("/app.js", "plain"); ("/app.js.gz", "gzip") ] in
  let response =
    Vitrine.handle only_gzip
      (request ~headers:[ ("Accept-Encoding", "gzip, br") ] "/app.js")
  in
  check_status Vitrine.Ok response;
  Alcotest.(check string) "encoding" "gzip" (header response "Content-Encoding");
  Alcotest.(check string) "body" "gzip" response.body

let compressed_response_keeps_original_mime () =
  let response =
    Vitrine.handle rich_store
      (request ~headers:[ ("Accept-Encoding", "br") ] "/app.js")
  in
  Alcotest.(check string) "mime" "text/javascript; charset=utf-8"
    (header response "Content-Type")

let spa_fallback () =
  let config = { Vitrine.default_config with spa_fallback = true } in
  let response = Vitrine.handle ~config rich_store (request "/client/route") in
  check_status Vitrine.Ok response;
  Alcotest.(check string) "body" "<h1>home</h1>" response.body

let unsupported_method () =
  let response = Vitrine.handle rich_store (request ~meth:(Vitrine.Other "POST") "/") in
  check_status Vitrine.Method_not_allowed response;
  Alcotest.(check string) "allow" "GET, HEAD" (header response "Allow")

let tests =
  [
    ("root resolves to index", `Quick, resolves_root);
    ("directory resolves to index", `Quick, resolves_directory_index);
    ("custom 404", `Quick, custom_404);
    ("reject traversal", `Quick, rejects_traversal);
    ("reject encoded traversal", `Quick, rejects_encoded_traversal);
    ("mime types", `Quick, mime_types);
    ("get body", `Quick, get_returns_body);
    ("head headers only", `Quick, head_returns_headers_only);
    ("etag emitted", `Quick, etag_emitted);
    ("if-none-match", `Quick, if_none_match_returns_304);
    ("immutable cache", `Quick, immutable_cache_for_hashed_assets);
    ("html cache", `Quick, html_cache_is_revalidate_friendly);
    ("brotli preferred", `Quick, brotli_preferred_over_gzip);
    ("gzip fallback", `Quick, gzip_used_when_brotli_unavailable);
    ("compressed mime", `Quick, compressed_response_keeps_original_mime);
    ("spa fallback", `Quick, spa_fallback);
    ("unsupported method", `Quick, unsupported_method);
  ]

let () = Alcotest.run "vitrine" [ ("static serving", tests) ]
