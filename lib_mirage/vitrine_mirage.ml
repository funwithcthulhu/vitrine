type route = {
  meth : Cohttp.Code.meth;
  path : string;
  handler :
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t;
}

let cohttp_status status =
  match Vitrine.status_to_int status with
  | 200 -> `OK
  | 304 -> `Not_modified
  | 400 -> `Bad_request
  | 404 -> `Not_found
  | 405 -> `Method_not_allowed
  | code -> `Code code

let response (vitrine_response : Vitrine.response) =
  let headers = Cohttp.Header.of_list vitrine_response.headers in
  let response =
    Cohttp.Response.make
      ~status:(cohttp_status vitrine_response.status)
      ~headers ()
  in
  (response, Cohttp_lwt.Body.of_string vitrine_response.body)

let vitrine_method = function
  | `GET -> Vitrine.Get
  | `HEAD -> Vitrine.Head
  | other -> Vitrine.Other (Cohttp.Code.string_of_method other)

let same_method a b =
  String.equal
    (Cohttp.Code.string_of_method a |> String.uppercase_ascii)
    (Cohttp.Code.string_of_method b |> String.uppercase_ascii)

let find_route routes request =
  let meth = Cohttp.Request.meth request in
  let path = request |> Cohttp.Request.uri |> Uri.path in
  List.find_opt
    (fun route -> same_method route.meth meth && String.equal route.path path)
    routes

let respond ?config ?(routes = []) store request body =
  match find_route routes request with
  | Some route -> route.handler request body
  | None ->
      let vitrine_request =
        {
          Vitrine.meth = vitrine_method (Cohttp.Request.meth request);
          path = request |> Cohttp.Request.uri |> Uri.path;
          headers = request |> Cohttp.Request.headers |> Cohttp.Header.to_list;
        }
      in
      Lwt.return (Vitrine.handle ?config store vitrine_request |> response)

let callback ?config ?routes store _conn request body =
  respond ?config ?routes store request body
