(** Cohttp/Lwt adapter for Vitrine. *)

type route = {
  meth : Cohttp.Code.meth;
  path : string;
  handler :
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t;
}

val response :
  Vitrine.response -> Cohttp.Response.t * Cohttp_lwt.Body.t

val respond :
  ?config:Vitrine.config ->
  ?routes:route list ->
  Vitrine.store ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val callback :
  ?config:Vitrine.config ->
  ?routes:route list ->
  Vitrine.store ->
  'conn ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
