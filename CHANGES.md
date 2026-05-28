# Changes

## 0.2.0

- Start conditional request handling for `If-Modified-Since` when embedded
  files include `Last-Modified` metadata.
- Accept weak `If-None-Match` validators during ETag revalidation.

## 0.1.0

- Add static-site serving logic with path normalization, cache headers, ETags,
  custom 404 pages, and precompressed asset selection.
- Add a Cohttp/Lwt adapter for MirageOS unikernels and local Unix serving.
- Add the `vitrine` command with `init`, `manifest`, `embed`, and `dev`
  commands.
- Add a small MirageOS example site and focused tests for the core serving
  behavior.
