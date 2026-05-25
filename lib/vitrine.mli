(** Static-site serving logic for Vitrine. *)

type meth = Get | Head | Other of string

type status =
  | Ok
  | Not_modified
  | Bad_request
  | Not_found
  | Method_not_allowed

type header = string * string

type request = {
  meth : meth;
  path : string;
  headers : header list;
}

type response = {
  status : status;
  headers : header list;
  body : string;
}

type file = {
  content : string;
  last_modified : string option;
}

type entry = {
  path : string;
  file : file;
}

type store = {
  get : string -> file option;
  exists : string -> bool;
  list : unit -> string list;
}

type config = {
  spa_fallback : bool;
  content_security_policy : string option;
  html_cache_control : string;
  immutable_cache_control : string;
  static_cache_control : string;
}

val default_config : config

type route = {
  meth : meth;
  path : string;
  handler : request -> response;
}

val handle : ?config:config -> ?routes:route list -> store -> request -> response

val text :
  ?status:status -> ?headers:header list -> string -> response

val json :
  ?status:status -> ?headers:header list -> string -> response

val status_to_int : status -> int
val status_reason : status -> string

val header : response -> string -> string option
val mime_type : string -> string
val hash : string -> string
val etag : string -> string

type cache_class = Html | Immutable | Static

val cache_class : string -> cache_class
val cache_control : ?config:config -> string -> string

type manifest_entry = {
  manifest_path : string;
  size : int;
  sha256 : string;
  manifest_mime_type : string;
  manifest_cache_class : cache_class;
  manifest_cache_control : string;
}

val manifest : ?config:config -> store -> manifest_entry list

module Memory_store : sig
  type t

  val of_entries : entry list -> t
  val store : t -> store
end
