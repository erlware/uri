{application, uri,
  [{description, "A "},
   {vsn, "0.1.0"},
   {modules, [
              crary,
              crary_app,
              crary_body,
              crary_headers,
              crary_ctrl,
              crary_port,
              crary_sock,
              crary_sup,
              crary_util,
              example
             ]},
   {registered,[crary_sup]},
   {applications, [kernel, stdlib]},
   {mod, {crary_app,[]}},
   {start_phases, []}]}.

