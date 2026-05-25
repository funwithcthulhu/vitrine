# Vitrine

Compile a site directory into a MirageOS web appliance.

Vitrine serves static files from a deterministic store and keeps the HTTP details in one place: path normalization, MIME types, ETags, cache headers, custom 404 pages, precompressed assets, and a small dynamic route hook.

## Quickstart

Build and test the package:

```sh
dune build @all
dune runtest
```

Serve the example site locally:

```sh
dune exec -- vitrine dev examples/basic-site/site --port 8080
```

Print the site manifest:

```sh
dune exec -- vitrine manifest examples/basic-site/site
```

Create a small project:

```sh
dune exec -- vitrine init my_site
```

## Layout

- `lib/` contains the runtime-independent static-serving logic.
- `lib_mirage/` adapts Vitrine responses to Cohttp/Lwt.
- `bin/` contains the `vitrine` command.
- `examples/basic-site/` contains a small site and Mirage entrypoint.
- `test/` covers the core serving behavior.

## Static Store

`vitrine embed site/` emits an OCaml module containing a `Vitrine.store`.

```sh
dune exec -- vitrine embed examples/basic-site/site > site_store.ml
```

The generated store is immutable OCaml data. It is intended for embedding into a unikernel image. Large sites may need a different store backend later; the current slice is aimed at small static sites.

## Mirage Build

The example uses Mirage's Cohttp server device:

```sh
cd examples/basic-site
mirage configure -t unix
mirage build
```

For Solo5:

```sh
mirage configure -t hvt
mirage build
```

This checkout does not vendor Mirage packages. Install Mirage, a Solo5 target, and the Cohttp Mirage stack in the switch used for unikernel builds.

## Albatross

Vitrine does not wrap deployment. For Albatross, build the `hvt` target and deploy the resulting Solo5 image with the tooling and network setup used on the target host.

## HTTP Behavior

Requests are normalized before lookup. Traversal attempts are rejected. `/` and directory paths resolve through `index.html`; unresolved paths use `/404.html` when present.

HTML uses `Cache-Control: no-cache`. Filenames with a hex hash segment use `public, max-age=31536000, immutable`. Other static assets use a short public cache lifetime.

If `file.br` or `file.gz` exists and the client advertises support, Vitrine serves the compressed file with the original file's MIME type. Brotli is preferred over gzip.

## Dynamic Routes

Static content is the default path. Tiny endpoints can be added at the adapter layer:

```ocaml
let routes =
  [
    {
      Vitrine_mirage.meth = `GET;
      path = "/health";
      handler =
        (fun _request _body ->
          Vitrine.text "ok\n" |> Vitrine_mirage.response |> Lwt.return);
    };
  ]
```
