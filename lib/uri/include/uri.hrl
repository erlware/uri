
-record(uri, {scheme,       %% "http", "ftp"
              user_info="", %% [] | "srp"
              host="",      %% "somewhere.net"
              port="",      %% [] | 80 | 8080
              path="",      %% "/here/there/everytwhere"
              raw_query="", %% "id=12345&name=fred+johnson". undecoded.
              frag="",      %% "some anchor"
              raw           %% original raw uri
             }).
