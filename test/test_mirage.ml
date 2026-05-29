let file content = { Vitrine.content; last_modified = None }

let store =
  Vitrine.Memory_store.(
    of_entries
      [
        { Vitrine.path = "/index.html"; file = file "<h1>home</h1>" };
        { Vitrine.path = "/404.html"; file = file "<h1>missing</h1>" };
      ]
    |> store)

let request ?(meth = `GET) path = Cohttp.Request.make ~meth (Uri.of_string path)
let body_to_string body = Lwt_main.run (Cohttp_lwt.Body.to_string body)

let check_response_status expected response =
  Alcotest.(check int)
    "status"
    (Vitrine.status_to_int expected)
    (response |> Cohttp.Response.status |> Cohttp.Code.code_of_status)

let response_maps_status_headers_and_body () =
  let response, body =
    Vitrine.text ~status:Vitrine.Not_found
      ~headers:[ ("X-Test", "yes") ]
      "missing\n"
    |> Vitrine_mirage.response
  in
  check_response_status Vitrine.Not_found response;
  Alcotest.(check (option string))
    "header" (Some "yes")
    (Cohttp.Header.get (Cohttp.Response.headers response) "X-Test");
  Alcotest.(check string) "body" "missing\n" (body_to_string body)

let dynamic_route_handles_health () =
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
  in
  let response, body =
    Lwt_main.run
      (Vitrine_mirage.respond ~routes store (request "/health")
         Cohttp_lwt.Body.empty)
  in
  check_response_status Vitrine.Ok response;
  Alcotest.(check string) "body" "ok\n" (body_to_string body)

let static_fallback_when_route_does_not_match () =
  let routes =
    [
      {
        Vitrine_mirage.meth = `GET;
        path = "/health";
        handler = (fun _ _ -> Alcotest.fail "route should not run");
      };
    ]
  in
  let response, body =
    Lwt_main.run
      (Vitrine_mirage.respond ~routes store (request "/") Cohttp_lwt.Body.empty)
  in
  check_response_status Vitrine.Ok response;
  Alcotest.(check string) "body" "<h1>home</h1>" (body_to_string body)

let unsupported_method_is_stable () =
  let response, body =
    Lwt_main.run
      (Vitrine_mirage.respond store (request ~meth:`POST "/")
         Cohttp_lwt.Body.empty)
  in
  check_response_status Vitrine.Method_not_allowed response;
  Alcotest.(check (option string))
    "allow" (Some "GET, HEAD")
    (Cohttp.Header.get (Cohttp.Response.headers response) "Allow");
  Alcotest.(check string) "body" "method not allowed\n" (body_to_string body)

let tests =
  [
    ("response conversion", `Quick, response_maps_status_headers_and_body);
    ("dynamic route", `Quick, dynamic_route_handles_health);
    ("static fallback", `Quick, static_fallback_when_route_does_not_match);
    ("unsupported method", `Quick, unsupported_method_is_stable);
  ]

let () = Alcotest.run "vitrine-mirage" [ ("mirage adapter", tests) ]
