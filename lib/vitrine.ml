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

let default_config =
  {
    spa_fallback = false;
    content_security_policy =
      Some "default-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'";
    html_cache_control = "no-cache";
    immutable_cache_control = "public, max-age=31536000, immutable";
    static_cache_control = "public, max-age=3600";
  }

type route = {
  meth : meth;
  path : string;
  handler : request -> response;
}

let status_to_int = function
  | Ok -> 200
  | Not_modified -> 304
  | Bad_request -> 400
  | Not_found -> 404
  | Method_not_allowed -> 405

let status_reason = function
  | Ok -> "OK"
  | Not_modified -> "Not Modified"
  | Bad_request -> "Bad Request"
  | Not_found -> "Not Found"
  | Method_not_allowed -> "Method Not Allowed"

let lower = String.lowercase_ascii

let header_of headers name =
  let name = lower name in
  List.find_map
    (fun (key, value) ->
      if String.equal (lower key) name then Some value else None)
    headers

let header response name = header_of response.headers name

let ensure_header key value headers =
  match header_of headers key with
  | Some _ -> headers
  | None -> headers @ [ (key, value) ]

let add_security_headers config headers =
  headers
  |> ensure_header "X-Content-Type-Options" "nosniff"
  |> ensure_header "Referrer-Policy" "no-referrer-when-downgrade"
  |> fun headers ->
  match config.content_security_policy with
  | None -> headers
  | Some value -> ensure_header "Content-Security-Policy" value headers

let text ?(status = Ok) ?(headers = []) body =
  {
    status;
    headers =
      headers
      |> ensure_header "Content-Type" "text/plain; charset=utf-8"
      |> ensure_header "Content-Length" (string_of_int (String.length body));
    body;
  }

let json ?(status = Ok) ?(headers = []) body =
  {
    status;
    headers =
      headers
      |> ensure_header "Content-Type" "application/json"
      |> ensure_header "Content-Length" (string_of_int (String.length body));
    body;
  }

let hash content =
  Digestif.SHA256.(content |> digest_string |> to_hex)

let etag content = "\"" ^ hash content ^ "\""

let has_suffix suffix s =
  let suffix_len = String.length suffix in
  let len = String.length s in
  len >= suffix_len && String.equal (String.sub s (len - suffix_len) suffix_len) suffix

let extension path =
  let basename =
    match List.rev (String.split_on_char '/' path) with
    | name :: _ -> name
    | [] -> path
  in
  match List.rev (String.split_on_char '.' basename) with
  | ext :: _ when not (String.equal ext basename) -> lower ext
  | _ -> ""

let mime_type path =
  match extension path with
  | "html" | "htm" -> "text/html; charset=utf-8"
  | "css" -> "text/css; charset=utf-8"
  | "js" | "mjs" -> "text/javascript; charset=utf-8"
  | "json" -> "application/json"
  | "txt" -> "text/plain; charset=utf-8"
  | "xml" -> "application/xml"
  | "png" -> "image/png"
  | "jpg" | "jpeg" -> "image/jpeg"
  | "gif" -> "image/gif"
  | "svg" -> "image/svg+xml"
  | "webp" -> "image/webp"
  | "ico" -> "image/x-icon"
  | "wasm" -> "application/wasm"
  | "pdf" -> "application/pdf"
  | "zip" -> "application/zip"
  | "gz" -> "application/gzip"
  | "br" -> "application/octet-stream"
  | "woff" -> "font/woff"
  | "woff2" -> "font/woff2"
  | "ttf" -> "font/ttf"
  | "otf" -> "font/otf"
  | _ -> "application/octet-stream"

type cache_class = Html | Immutable | Static

let is_hex s =
  let len = String.length s in
  len >= 8
  &&
  let rec loop i =
    i = len
    ||
    match s.[i] with
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> loop (i + 1)
    | _ -> false
  in
  loop 0

let filename_tokens path =
  let basename =
    match List.rev (String.split_on_char '/' path) with
    | name :: _ -> name
    | [] -> path
  in
  let split_chars = function '.' | '-' | '_' -> true | _ -> false in
  let rec loop acc start i =
    if i = String.length basename then
      let token = String.sub basename start (i - start) in
      List.rev (if String.equal token "" then acc else token :: acc)
    else if split_chars basename.[i] then
      let token = String.sub basename start (i - start) in
      let acc = if String.equal token "" then acc else token :: acc in
      loop acc (i + 1) (i + 1)
    else loop acc start (i + 1)
  in
  loop [] 0 0

let is_hashed_asset path =
  List.exists is_hex (filename_tokens path)

let cache_class path =
  match extension path with
  | "html" | "htm" -> Html
  | _ when is_hashed_asset path -> Immutable
  | _ -> Static

let cache_control ?(config = default_config) path =
  match cache_class path with
  | Html -> config.html_cache_control
  | Immutable -> config.immutable_cache_control
  | Static -> config.static_cache_control

type normalized_path = {
  path : string;
  directory : bool;
}

type path_error = Traversal | Invalid_path

let normalize_path raw_path =
  let path =
    match String.split_on_char '?' raw_path with
    | path :: _ -> path
    | [] -> raw_path
  in
  let decoded =
    try Some (Uri.pct_decode path) with
    | Invalid_argument _ -> None
  in
  match decoded with
  | None -> Error Invalid_path
  | Some decoded ->
      if String.exists (fun ch -> Char.equal ch '\000' || Char.equal ch '\\') decoded then
        Error Invalid_path
      else
        let directory = String.equal decoded "" || has_suffix "/" decoded in
        let decoded =
          if String.equal decoded "" then "/"
          else if Char.equal decoded.[0] '/' then decoded
          else "/" ^ decoded
        in
        let segments =
          decoded
          |> String.split_on_char '/'
          |> List.filter (fun segment -> not (String.equal segment ""))
        in
        if List.exists (fun segment -> String.equal segment "." || String.equal segment "..") segments then
          Error Traversal
        else
          let path =
            match segments with
            | [] -> "/"
            | _ -> "/" ^ String.concat "/" segments
          in
          Ok { path; directory }

let candidate_paths normalized =
  match (normalized.path, normalized.directory) with
  | "/", _ -> [ "/index.html" ]
  | path, true -> [ path ^ "/index.html" ]
  | path, false -> [ path; path ^ "/index.html" ]

type selected_encoding =
  | Identity
  | Brotli
  | Gzip

let encoding_header = function
  | Identity -> None
  | Brotli -> Some "br"
  | Gzip -> Some "gzip"

let trim s =
  let len = String.length s in
  let rec left i =
    if i = len then len
    else
      match s.[i] with
      | ' ' | '\t' -> left (i + 1)
      | _ -> i
  in
  let rec right i =
    if i < 0 then -1
    else
      match s.[i] with
      | ' ' | '\t' -> right (i - 1)
      | _ -> i
  in
  let left = left 0 in
  let right = right (len - 1) in
  if right < left then "" else String.sub s left (right - left + 1)

let parse_month = function
  | "Jan" -> Some 1
  | "Feb" -> Some 2
  | "Mar" -> Some 3
  | "Apr" -> Some 4
  | "May" -> Some 5
  | "Jun" -> Some 6
  | "Jul" -> Some 7
  | "Aug" -> Some 8
  | "Sep" -> Some 9
  | "Oct" -> Some 10
  | "Nov" -> Some 11
  | "Dec" -> Some 12
  | _ -> None

let leap_year year =
  (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let days_before_month year month =
  let common =
    [| 0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334 |]
  in
  let days = common.(month - 1) in
  if month > 2 && leap_year year then days + 1 else days

let leaps_before year =
  let year = year - 1 in
  (year / 4) - (year / 100) + (year / 400)

let days_before_year year =
  (365 * (year - 1970)) + leaps_before year - leaps_before 1970

let days_in_month year month =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if leap_year year then 29 else 28
  | _ -> 0

let parse_time s =
  match String.split_on_char ':' s with
  | [ hour; minute; second ] -> (
      try Some (int_of_string hour, int_of_string minute, int_of_string second)
      with Failure _ -> None)
  | _ -> None

let http_date_seconds value =
  match String.split_on_char ' ' (trim value) with
  | [ _weekday; day; month; year; time; "GMT" ] -> (
      match (parse_month month, parse_time time) with
      | Some month, Some (hour, minute, second) -> (
          try
            let day = int_of_string day in
            let year = int_of_string year in
            if
              year >= 1970
              && day >= 1
              && day <= days_in_month year month
              && hour >= 0
              && hour <= 23
              && minute >= 0
              && minute <= 59
              && second >= 0
              && second <= 60
            then
              let days =
                days_before_year year + days_before_month year month + day - 1
              in
              Some ((days * 86_400) + (hour * 3_600) + (minute * 60) + second)
            else None
          with Failure _ -> None)
      | _ -> None)
  | _ -> None

let is_zero_q param =
  match String.split_on_char '=' param with
  | [ name; value ] when String.equal (lower (trim name)) "q" -> (
      try Float.equal (float_of_string (trim value)) 0.0 with
      | Failure _ -> false)
  | _ -> false

let accepts_encoding (request : request) name =
  match header_of request.headers "Accept-Encoding" with
  | None -> false
  | Some value ->
      value
      |> String.split_on_char ','
      |> List.exists (fun part ->
             match String.split_on_char ';' part with
             | token :: params ->
                 let token = lower (trim token) in
                 let q_zero =
                   List.exists (fun param -> is_zero_q (trim param)) params
                 in
                 (String.equal token name || String.equal token "*") && not q_zero
             | [] -> false)

let select_encoding store request path =
  if accepts_encoding request "br" && store.exists (path ^ ".br") then
    (Brotli, path ^ ".br")
  else if accepts_encoding request "gzip" && store.exists (path ^ ".gz") then
    (Gzip, path ^ ".gz")
  else (Identity, path)

let if_none_match_matches (request : request) tag =
  match header_of request.headers "If-None-Match" with
  | None -> false
  | Some value ->
      value
      |> String.split_on_char ','
      |> List.map trim
      |> List.exists (fun candidate -> String.equal candidate "*" || String.equal candidate tag)

let if_modified_since_matches (request : request) file =
  match (header_of request.headers "If-Modified-Since", file.last_modified) with
  | Some since_, Some modified -> (
      match (http_date_seconds since_, http_date_seconds modified) with
      | Some since_, Some modified -> modified <= since_
      | _ -> false)
  | _ -> false

let not_modified (request : request) tag (file : file) =
  match header_of request.headers "If-None-Match" with
  | Some _ -> if_none_match_matches request tag
  | None -> if_modified_since_matches request file

let method_allows_static = function
  | Get | Head -> true
  | Other _ -> false

let method_name = function
  | Get -> "GET"
  | Head -> "HEAD"
  | Other name -> String.uppercase_ascii name

let same_method a b = String.equal (method_name a) (method_name b)

let response_with_body ~config ~(request : request) ~status ~path
    ?content_encoding file =
  let tag = etag file.content in
  let not_modified = not_modified request tag file in
  let base_headers =
    [
      ("Content-Type", mime_type path);
      ("ETag", tag);
      ("Cache-Control", cache_control ~config path);
    ]
  in
  let base_headers =
    match file.last_modified with
    | None -> base_headers
    | Some value -> base_headers @ [ ("Last-Modified", value) ]
  in
  let base_headers =
    match content_encoding with
    | None -> base_headers
    | Some value -> base_headers @ [ ("Content-Encoding", value); ("Vary", "Accept-Encoding") ]
  in
  if not_modified then
    {
      status = Not_modified;
      headers = add_security_headers config base_headers;
      body = "";
    }
  else
    let body =
      match request.meth with
      | Head -> ""
      | Get | Other _ -> file.content
    in
    {
      status;
      headers =
        base_headers
        |> ensure_header "Content-Length" (string_of_int (String.length file.content))
        |> add_security_headers config;
      body;
    }

let plain_response ~config ~(request : request) ~status ~body =
  let body_for_method =
    match request.meth with
    | Head -> ""
    | Get | Other _ -> body
  in
  {
    status;
    headers =
      [
        ("Content-Type", "text/plain; charset=utf-8");
        ("Cache-Control", "no-cache");
        ("Content-Length", string_of_int (String.length body));
      ]
      |> add_security_headers config;
    body = body_for_method;
  }

let method_not_allowed ~config request =
  let response = plain_response ~config ~request ~status:Method_not_allowed ~body:"method not allowed\n" in
  { response with headers = response.headers @ [ ("Allow", "GET, HEAD") ] }

let custom_404 ~config store request =
  match store.get "/404.html" with
  | Some file -> response_with_body ~config ~request ~status:Not_found ~path:"/404.html" file
  | None -> plain_response ~config ~request ~status:Not_found ~body:"not found\n"

let bad_request ~config request =
  plain_response ~config ~request ~status:Bad_request ~body:"bad request\n"

let finalize_route_response ~config (request : request) response =
  let body =
    match request.meth with
    | Head -> ""
    | Get | Other _ -> response.body
  in
  let headers =
    response.headers
    |> ensure_header "Content-Length" (string_of_int (String.length response.body))
    |> add_security_headers config
  in
  { response with headers; body }

let find_route routes meth path =
  List.find_map
    (fun route ->
      if same_method route.meth meth && String.equal route.path path then Some route.handler
      else None)
    routes

let find_static store normalized =
  List.find_map
    (fun path ->
      match store.get path with
      | Some file -> Some (path, file)
      | None -> None)
    (candidate_paths normalized)

let handle ?(config = default_config) ?(routes = []) store (request : request) =
  match normalize_path request.path with
  | Error _ -> bad_request ~config request
  | Ok normalized -> (
      match find_route routes request.meth normalized.path with
      | Some handler -> handler request |> finalize_route_response ~config request
      | None when not (method_allows_static request.meth) -> method_not_allowed ~config request
      | None -> (
          match find_static store normalized with
          | Some (path, _) ->
              let encoding, stored_path = select_encoding store request path in
              let file =
                match store.get stored_path with
                | Some file -> file
                | None -> Option.get (store.get path)
              in
              response_with_body ~config ~request ~status:Ok ~path
                ?content_encoding:(encoding_header encoding) file
          | None when config.spa_fallback && same_method request.meth Get -> (
              match store.get "/index.html" with
              | Some file -> response_with_body ~config ~request ~status:Ok ~path:"/index.html" file
              | None -> custom_404 ~config store request)
          | None -> custom_404 ~config store request))

type manifest_entry = {
  manifest_path : string;
  size : int;
  sha256 : string;
  manifest_mime_type : string;
  manifest_cache_class : cache_class;
  manifest_cache_control : string;
}

let manifest ?(config = default_config) store =
  store.list ()
  |> List.sort_uniq String.compare
  |> List.filter_map (fun path ->
         match store.get path with
         | None -> None
         | Some file ->
             Some
               {
                 manifest_path = path;
                 size = String.length file.content;
                 sha256 = hash file.content;
                 manifest_mime_type = mime_type path;
                 manifest_cache_class = cache_class path;
                 manifest_cache_control = cache_control ~config path;
               })

module Path_map = Map.Make (String)

module Memory_store = struct
  type t = file Path_map.t

  let normalize_entry_path path =
    match normalize_path path with
    | Ok { path; _ } -> path
    | Error _ -> invalid_arg ("invalid store path: " ^ path)

  let of_entries entries =
    List.fold_left
      (fun acc (entry : entry) -> Path_map.add (normalize_entry_path entry.path) entry.file acc)
      Path_map.empty entries

  let store files =
    {
      get = (fun path -> Path_map.find_opt path files);
      exists = (fun path -> Path_map.mem path files);
      list = (fun () -> files |> Path_map.bindings |> List.map fst);
    }
end
