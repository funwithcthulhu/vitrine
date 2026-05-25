module Make (Server : Cohttp_lwt.S.Server) = struct
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

  let start server =
    let callback = Vitrine_mirage.callback ~routes Site_store.store in
    Server.callback (Server.make ~callback ()) server
end
