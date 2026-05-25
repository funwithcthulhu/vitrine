let store =
  Vitrine.Memory_store.of_entries
    [
      {
        Vitrine.path = "/404.html";
        file =
          {
            Vitrine.content =
              "<!doctype html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"utf-8\">\n  <title>Not found</title>\n</head>\n<body>\n  <h1>Not found</h1>\n  <p>The requested page does not exist.</p>\n</body>\n</html>\n";
            last_modified = None;
          };
      };
      {
        Vitrine.path = "/app.js";
        file =
          {
            Vitrine.content = "document.documentElement.dataset.vitrine = \"ready\";\n";
            last_modified = None;
          };
      };
      {
        Vitrine.path = "/asset.0123456789abcdef.txt";
        file =
          {
            Vitrine.content =
              "This filename contains a content-style hash segment for cache policy tests.\n";
            last_modified = None;
          };
      };
      {
        Vitrine.path = "/index.html";
        file =
          {
            Vitrine.content =
              "<!doctype html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"utf-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <link rel=\"stylesheet\" href=\"/style.css\">\n  <title>Vitrine basic site</title>\n</head>\n<body>\n  <main>\n    <h1>Vitrine basic site</h1>\n    <p>This page is served from an embedded site directory.</p>\n    <p><a href=\"/asset.0123456789abcdef.txt\">Hashed asset</a></p>\n  </main>\n  <script src=\"/app.js\"></script>\n</body>\n</html>\n";
            last_modified = None;
          };
      };
      {
        Vitrine.path = "/style.css";
        file =
          {
            Vitrine.content =
              "html {\n  font-family: system-ui, sans-serif;\n  color: #202020;\n  background: #f7f7f4;\n}\n\nbody {\n  margin: 0;\n}\n\nmain {\n  max-width: 42rem;\n  margin: 4rem auto;\n  padding: 0 1rem;\n}\n";
            last_modified = None;
          };
      };
    ]
  |> Vitrine.Memory_store.store
