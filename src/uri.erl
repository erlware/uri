%%% Copyright (c) 2007, 2008 Scott Parish
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.

%%%-------------------------------------------------------------------

%%% @author Scott Parish <srp@srparish.net>
%%% @copyright 2007, 2008 Scott Parish <srp@srparish.net>
%%% @doc A module for generating, parsing, encoding, and decoding uris.
%%%
%%% At the moment this module isn't very sympathetic to non-http
%%% uri's, but that could/should change in the future.

-module(uri).

-export([new/7, new/8, from_string/1, from_http_1_1/3, to_string/1]).
-export([query_foldl/3]).
-export([query_to_dict/1, query_to_tl/1]).
-export([to_query/1, to_query/2]).
-export([quote/1, quote/2]).
-export([unquote/1]).
-export([scheme/1, scheme/2, user_info/1, user_info/2, host/1, host/2]).
-export([port/1, port/2, path/1, path/2, raw_query/1, raw_query/2]).
-export([frag/1, frag/2, raw/1]).

-include("eunit.hrl").
-include("uri.hrl").

%% @type uri() = record().
%%       This is a record that represents the different parts of a uri,
%%       as defined by rfc-2396. It has the following fields:
%%       <dl>
%%        <dt>scheme::string()</dt>
%%        <dd>`"http"', `"https"', `"ftp"', etc</dd>
%%
%%        <dt>user_info::string()</dt>
%%        <dd>This will be `"parish:secret"' for the uri
%%            `"http://parish:secret@somehost.com/index.html"'</dd>
%%
%%        <dt>host::string()</dt>
%%        <dd>This will be `"somehost.com"' for the uri
%%            `"http://somehost.com/index.html"'.</dd>
%%
%%        <dt>port::integer() | []</dt>
%%        <dd>This will be `8080' for the uri
%%            `"http://somehost.com:8080/index.html"', and `[]' for
%%            uri `"http://somehost.com/index.html"'.</dd>
%%
%%        <dt>path::string()</dt>
%%        <dd>This will be `"/index.html"' for the uri
%%            `"http://somehost.com/index.html?startId=50"'. This will
%%            be unquoted, so `"http://somehost.com/name+with%20spaces"'
%%            will be `"/name with spaces"'</dd>
%%
%%        <dt>raw_query::iolist()</dt>
%%        <dd>This is the not unquoted. Probably the most convient way to
%%            access the query componant will be to use {@link query_to_tl/1}
%%            or {@link query_to_dict/1}. The value will be the empty string
%%            if no query was found in the uri.</dd>
%%
%%        <dt>frag::string()</dt>
%%        <dd>The fragment part of the url, unquoted. This will be
%%            `"Section 5"' for the uri
%%            `"http://somehost.com/index.html#Section+5"'. It will be
%%            The empty string if no fragment is found.</dd>
%%
%%        <dt>raw::string()</dt>
%%        <dd>This is the original uri that the above fields were populated
%%            from. Everything will still be in their original quoted form.
%%            Note that this may be a best guess as to the uri a user had
%%            in their browser, as this will most likely be formed by
%%            concatenating the `Host' header with the `request' line uri.</dd>
%%       </dl>


%% @doc Populate a new #uri record by parsing the string `Uri'
%% @spec(string()) -> uri()
from_string(Uri) ->
    {Scheme, Uri1} = parse_scheme(Uri),

    {Authority, Uri2} = parse_authority(Uri1),
    {UserInfo, HostPort} = parse_user_info(Authority),
    {Host, Port} = parse_host_port(HostPort),

    {Path, Uri3} = parse_path(Uri2),
    {Query, Uri4} = parse_query(Uri3),
    Frag = parse_frag(Uri4),
    new(Scheme, UserInfo, Host, Port, Path, Query, Frag, Uri).

%% @doc Return the string this uri represents. (Same as the `raw' field)
%% @spec(uri()) -> string()
to_string(#uri{raw = Raw}) ->
    Raw.

%% @doc Populate a new #uri record by using `Scheme' and parsing
%% `HostPort' string `Uri'
%% @spec(string(), string(), string()) -> uri()
from_http_1_1(Scheme, HostPort, Uri) ->
    {Host, Port} = parse_host_port(HostPort),
    {Path, Uri1} = parse_path(Uri),
    {Query, Uri2} = parse_query(Uri1),
    Frag = parse_frag(Uri2),
    new(Scheme, "", Host, Port, Path, Query, Frag).

%% @doc Return a uri record with the given fields. Use `""' for any field
%% that isn't used.
%%
%% You probably want {@link raw/7} unless you've parsed a uri yourself.
%% @spec(string(), string(), string(), integer(), string(),
%%       string(), string(), string()) -> uri()
new(Scheme, UserInfo, Host, Port, Path, Query, Frag, Uri) ->
    update_raw(#uri{scheme = Scheme,
                    user_info = unquote(UserInfo),
                    host = Host,
                    port = Port,
                    path = unquote(Path),
                    raw_query = Query,
                    frag = unquote(Frag),
                    raw = Uri}).

%% @doc Return a uri record with the given fields. Use `""' for any field
%% that isn't used.
%% @spec(string(), string(), string(), integer(), string(),
%%       string(), string()) -> uri()
new(Scheme, UserInfo, Host, Port, Path, Query, Frag) ->
    update_raw(#uri{scheme = Scheme,
                    user_info = unquote(UserInfo),
                    host = Host,
                    port = Port,
                    path = unquote(Path),
                    raw_query = Query,
                    frag = unquote(Frag)}).

new_test() ->
    ?assertMatch("http://myhost.com:8080/my/path?color=red#Section%205",
                 to_string(new("http", "", "myhost.com", 8080, "/my/path",
                               "color=red", "Section 5"))).

parse_scheme(Uri) ->
    parse_scheme(Uri, []).

parse_scheme([$: | Uri], Acc) ->
    {lists:reverse(Acc), Uri};
parse_scheme([], Acc) ->
    {[], lists:reverse(Acc)};
parse_scheme([C | Rest], Acc) ->
    parse_scheme(Rest, [C | Acc]).

parse_scheme_test() ->
    ?assertMatch({"http", "//test.com/"}, parse_scheme("http://test.com/")),
    ?assertMatch({"", "/test"}, parse_scheme("/test")),
    ?assertMatch({"mailto", "x@test.com"}, parse_scheme("mailto:x@test.com")).

parse_authority("//" ++ Uri) ->
    parse_authority(Uri, "");
parse_authority(Uri) ->
    Uri.

parse_authority([$/ | Rest], Acc) ->
    {lists:reverse(Acc), [$/ | Rest]};
parse_authority([], Acc) ->
    {lists:reverse(Acc), []};
parse_authority([C | Rest], Acc) ->
    parse_authority(Rest, [C | Acc]).

parse_authority_test() ->
    ?assertMatch({"test.com", "/here"}, parse_authority("//test.com/here")),
    ?assertMatch({"test.com", ""}, parse_authority("//test.com")),
    ?assertMatch({"", "/test"}, parse_scheme("/test")).

parse_user_info(Authority) ->
    parse_user_info(Authority, []).

parse_user_info([$@ | HostPort], Acc) ->
    {lists:reverse(Acc), HostPort};
parse_user_info([], Acc) ->
    {[], lists:reverse(Acc)};
parse_user_info([C | HostPort], Acc) ->
    parse_user_info(HostPort, [C | Acc]).

parse_user_info_test() ->
    ?assertMatch({"user", "test.com"}, parse_user_info("user@test.com")),
    ?assertMatch({"", "user.test.com"}, parse_user_info("user.test.com")).

parse_host_port(HostPort) ->
    case string:tokens(HostPort, ":") of
        [Host] -> {Host, ""};
        [Host, Port] -> {Host, list_to_integer(Port)};
        _ -> throw({uri_error, {invalid_host_port, HostPort}})
    end.

parse_host_port_test() ->
    ?assertMatch({"test.com", 8080}, parse_host_port("test.com:8080")),
    ?assertMatch({"test.com", ""}, parse_host_port("test.com")).

parse_path(Uri) ->
    parse_path(Uri, []).

parse_path([C | Uri], Acc) when C == $?; C == $# ->
    {lists:reverse(Acc), [C | Uri]};
parse_path([], Acc) ->
    {lists:reverse(Acc), ""};
parse_path([C | Uri], Acc) ->
    parse_path(Uri, [C | Acc]).

parse_path_test() ->
    ?assertMatch({"/a/b/c", ""}, parse_path("/a/b/c")),
    ?assertMatch({"/a/b/c", "?n=5"}, parse_path("/a/b/c?n=5")),
    ?assertMatch({"/a/b/c", "#anchor"}, parse_path("/a/b/c#anchor")),
    ?assertMatch({"", ""}, parse_path("")).

parse_query([$? | Uri]) ->
    parse_query(Uri, []);
parse_query(Uri) ->
    {"", Uri}.

parse_query([$# | Uri], Acc) ->
    {lists:reverse(Acc), [$# | Uri]};
parse_query([], Acc) ->
    {lists:reverse(Acc), ""};
parse_query([C | Rest], Acc) ->
    parse_query(Rest, [C | Acc]).

parse_query_test() ->
    ?assertMatch({"a=b", ""}, parse_query("?a=b")),
    ?assertMatch({"a=b", "#anchor"}, parse_query("?a=b#anchor")),
    ?assertMatch({"", "#anchor"}, parse_query("#anchor")),
    ?assertMatch({"", ""}, parse_query("")).

parse_frag([$# | Frag]) ->
    unquote(Frag);
parse_frag("") ->
    "";
parse_frag(Data) ->
    throw({uri_error, {data_left_after_parsing, Data}}).

user_info_to_string(#uri{user_info = ""}) ->
    "";
user_info_to_string(#uri{user_info = UserInfo}) ->
    [UserInfo, $@].

port_to_string(#uri{port = ""}) ->
    "";
port_to_string(#uri{port = Port}) ->
    [$: | integer_to_list(Port)].

path_to_string(#uri{path = ""}) ->
    $/;
path_to_string(#uri{path = Path}) ->
    quote(Path, path).

raw_query_to_string(#uri{raw_query = ""}) ->
    "";
raw_query_to_string(#uri{raw_query = RawQuery}) ->
    [$? | RawQuery].

frag_to_string(#uri{frag = ""}) ->
    "";
frag_to_string(#uri{frag = Frag}) ->
    [$#, quote(Frag, frag)].

%% @doc Convert the string or the `raw_query' portion of {@link uri()} into
%%      a dictionary, where the keys are strings, the values are strings,
%%      and for valueless keys, the atom `true' is used as the value.
%%
%%      For example, `"range=5-50&printable"' would result in the following
%%      dictionary entries:
%%      <table>
%%       <tr><th>Key</th><th>Value</th></tr>
%%       <tr><td>"range"</td><td>"5-50"</td></tr>
%%       <tr><td>"printable"</td><td>true</td></tr>
%%      </table>
%%
%%      The string needent have to be from a uri, this method is also
%%      useful for decoding the `Post' body of an HTTP form submission.
%% @todo make a form of this that for keys specified multiple times, return
%%       them as some kind of a list. | Actually, maybe this should be pushed
%%       to the tuple-list/dict libraries to support multiple values for a
%%       single key.
%% @spec(Query) -> dict().
%%       Query = string() | uri()
query_to_dict(Query) ->
    query_foldl(fun ({K, V}, D) -> dict:store(K, V, D) end, dict:new(), Query).

%% @doc Convert the string or the `raw_query' portion of {@link uri()} into
%%      a tuple-list. See {@link query_to_dict/1} for more information, this
%%      is basically the same except that for duplicate keys,
%%      {@link query_to_dict/1} will currently overwrite earlier values where
%%      this will return a tuple-list with multiple key entries.
%% @see query_to_dict/1
%% @spec(Query) -> dict().
%%       Query = string() | uri()
query_to_tl(Query) ->
    lists:reverse(query_foldl(fun (KV, Acc) -> [KV | Acc] end, [], Query)).

query_to_tl_test() ->
    ?assertMatch([{"a", true}, {"b", "c"}], query_to_tl("a&b=c")).

%% @doc Fold over each element of a query. For instance with the
%%      query `"range=5-50&printable"', `F' will be called as
%%      `F("range", "5-50", Acc)' and `F("printable", true, Acc)'.
%%      Both `Key' and `Value' are already unquoted when `F' is called.
%% @see query_to_dict/1
%% @spec(F::function(), term(), Query) -> term().
%%       Query = string() | uri()
query_foldl(F, Init, #uri{raw_query = Query}) ->
    query_foldl(F, Init, Query);
query_foldl(F, Init, Query) ->
    lists:foldl(fun (Part, Acc) ->
                        case string:tokens(Part, "=") of
                            [Key, Value] ->
                                F({unquote(Key), unquote(Value)}, Acc);
                            [Key] ->
                                F({unquote(Key), true}, Acc)
                        end
                end, Init, string:tokens(iolist_to_string(Query), "&")).

iolist_to_string(Str) ->
    binary_to_list(iolist_to_binary(Str)).

%% @doc Convert a dictionary or tuple-list to an iolist representing the
%% query part of a uri. Keys and values can be binaries, lists, atoms,
%% integers or floats, and will be automatically converted to a string and
%% quoted.
%% @spec(KeyValues) -> iolist().
%%     KeyValues = dict() | tuple_list().
to_query({dict,_,_,_,_,_,_,_,_} = Dict) ->
    to_query(fun dict:fold/3, Dict);
to_query(List) ->
    to_query(fun lists:foldl/3, List).

%% @doc Return an iolist representing the query part of a uri by
%% folding over `Ds' by calling the provided `FoldF', which should
%% take three arguments: a function, an initial accumulator value, and
%% the datastructure to fold over.
%% @see to_query/1
%% @spec(function(), term()) -> iolist().
to_query(FoldF, Ds) ->
    string_join($&,
                FoldF(fun ({K, V}, Acc) ->
                              [[quote(el_to_string(K), 'query'), $=,
                                quote(el_to_string(V), 'query')] | Acc];
                          (K, Acc) ->
                              [quote(el_to_string(K), 'query') | Acc]
                      end, [], Ds)).

to_query_test() ->
    ?assertMatch(
       "one&two=2&three=two%20%2B%20one",
       iolist_to_string(to_query([one, {"two", 2}, {"three", "two + one"}]))).


el_to_string(El) when is_atom(El) ->
    atom_to_list(El);
el_to_string(El) when is_number(El) ->
    integer_to_list(El);
el_to_string(El) when is_float(El) ->
    float_to_list(El);
el_to_string(El) ->
    El.

string_join(_, []) ->
    "";
string_join(_, [Str]) ->
    Str;
string_join(Join, List) ->
    lists:foldl(fun (Str, Acc) -> [Str, Join | Acc] end, hd(List), tl(List)).


%% @doc Return `Str' with all `+' replaced with space, and `%NN' replaced
%%      with the decoded byte.
%% @spec(string()) -> string().
unquote(Str) ->
    unquote(Str, []).

unquote_test() ->
    ?assertMatch("ab", unquote("ab")),
    ?assertMatch("a b", unquote("a+b")),
    ?assertMatch("a b", unquote("a%20b")).

unquote([], Acc) ->
    lists:reverse(Acc);
unquote([$+ | Str], Acc) ->
    unquote(Str, [$  | Acc]);
unquote([$\%, A, B | Str], Acc) ->
    unquote(Str, [erlang:list_to_integer([A, B], 16) | Acc]);
unquote([C | Str], Acc) ->
    unquote(Str, [C | Acc]).

%% @doc Return `Str' with all reserved or uri-unsafe characters in
%%      quoted form. For instance `"A Space"' becomes `"A%20Space"'.
%%      This is the same as calling `quote(Str, any)'.
%% @see quote/2
%% @spec(string()) -> string().
quote(Str) ->
    quote(Str, any).

%% @doc Return `Str' with all reserved or uri-unsafe characters in
%%      quoted form. Since rfc-2396 has different reserved characters
%%      for different parts of the uri, you can specify `Part' to
%%      obtain a minimally quoted uri for that `Part'.
%%
%%      `Part' can be one of the following values:
%%      <dl>
%%       <dt>any</dt>
%%       <dd>Quote any character that is reserved in any potential part.</dd>
%%
%%       <dt>userinfo</dt>
%%       <dd>Quote for the userinfo part of a uri.</dd>
%%
%%       <dt>path</dt>
%%       <dd>Quote for the path part of a uri.</dd>
%%
%%       <dt>segment</dt>
%%       <dd>Quote for a path's segment. This is much like `path', but
%%           will also quote the `/' character.</dd>
%%
%%       <dt>segment_param</dt>
%%       <dd>Quote for path segment's parameters (a fairly obscure part
%%           of uris). This is like `path' but will also quote the characters
%%           `/' and `;'.</dd>
%%
%%       <dt>query_ | 'query'</dt>
%%       <dd>Quote for query parts. `query' is an erlang keyword so you can
%%           either use `` 'query' '' or `query_' to specify this part. This
%%           will quote characters such as `&' and `=', so it needs to be
%%           called on the individual key/value parts of a query. See
%%           {@link dict_to_query/1} and {@link tl_to_query/1}.</dd>
%%
%%       <dt>frag</dt>
%%       <dd>Quote for the fragment part of a uri</dd>
%%      </dl>
%% @spec(string(), atom()) -> string()
quote(Str, Part) ->
    lists:reverse(lists:foldl(fun (C, Acc) ->
                                      [escape_for_part(C, Part) | Acc]
                              end, [], Str)).

quote_test() ->
    ?assertMatch("abc123", lists:flatten(quote("abc123"))),
    ?assertMatch("abc%20123", lists:flatten(quote("abc 123"))).

escape_for_part(C, Part) ->
    case case Part of
             any           -> is_unreserved(C);
             userinfo      -> is_userinfo(C);
             path          -> is_pchar(C) orelse C == $; orelse C == $/;
             segment       -> is_pchar(C) orelse C == $;;
             segment_param -> is_pchar(C);
             query_        -> is_unreserved(C);
             'query'       -> is_unreserved(C);
             fragment      -> is_unreserved(C);
             frag          -> is_unreserved(C)
         end of
        true -> C;
        false -> escape(C)
    end.

escape(C) ->
    io_lib:format("%~2.16.0B", [C]).

escape_test() ->
    ?assertMatch("%20", lists:flatten(escape($ ))).

is_unreserved(C) -> is_alphanum(C) orelse is_mark(C).
is_alphanum(C)   -> is_alpha(C) orelse is_digit(C).
is_alpha(C)      -> is_lowalpha(C) orelse is_upalpha(C).
is_lowalpha(C)   -> $a =< C andalso C =< $z.
is_upalpha(C)    -> $A =< C andalso C =< $Z.
is_digit(C)      -> $0 =< C andalso C =< $9.

is_pchar($:) -> true;
is_pchar($@) -> true;
is_pchar($&) -> true;
is_pchar($=) -> true;
is_pchar($+) -> true;
is_pchar($$) -> true;
is_pchar($,) -> true;
is_pchar(C)  -> is_unreserved(C).

is_userinfo($;) -> true;
is_userinfo($:) -> true;
is_userinfo($&) -> true;
is_userinfo($=) -> true;
is_userinfo($+) -> true;
is_userinfo($$) -> true;
is_userinfo($,) -> true;
is_userinfo(C)  -> is_unreserved(C).

is_mark($-) -> true;
is_mark($_) -> true;
is_mark($.) -> true;
is_mark($!) -> true;
is_mark($~) -> true;
is_mark($*) -> true;
is_mark($\') -> true;
is_mark($() -> true;
is_mark($)) -> true;
is_mark(_) -> false.

%% @doc Return the scheme field of {@link uri()}.
%% @spec(uri()) -> string()
scheme(#uri{scheme = Scheme}) ->
    Scheme.

%% @doc Set the scheme field of {@link uri()}.
%% @spec(string()) -> uri()
scheme(Uri, NewScheme) ->
    update_raw(Uri#uri{scheme = NewScheme}).

%% @doc Return the user_info field of {@link uri()}.
%% @spec(uri()) -> string()
user_info(#uri{user_info = UserInfo}) ->
    UserInfo.

%% @doc Set the user_info field of {@link uri()}.
%% @spec(string()) -> uri()
user_info(Uri, NewUserInfo) ->
    update_raw(Uri#uri{user_info = NewUserInfo}).

%% @doc Return the host field of {@link uri()}.
%% @spec(uri()) -> string()
host(#uri{host = Host}) ->
    Host.

%% @doc Set the host field of {@link uri()}.
%% @spec(string()) -> uri()
host(Uri, NewHost) ->
    update_raw(Uri#uri{host = NewHost}).

%% @doc Return the port field of {@link uri()}.
%% @spec(uri()) -> integer()
port(#uri{port = Port}) ->
    Port.

%% @doc Set the port field of {@link uri()}.
%% @spec(integer()) -> uri()
port(Uri, NewPort) ->
    update_raw(Uri#uri{port = NewPort}).

%% @doc Return the path field of {@link uri()}.
%% @spec(uri()) -> string()
path(#uri{path = Path}) ->
    Path.

%% @doc Set the path field of {@link uri()}.
%% @spec(string()) -> uri()
path(Uri, NewPath) ->
    update_raw(Uri#uri{path = NewPath}).

%% @doc Return the raw_query field of {@link uri()}.
%% @spec(uri()) -> string()
raw_query(#uri{raw_query = RawQuery}) ->
    RawQuery.

%% @doc Set the raw_query field of {@link uri()}.
%% @spec(string()) -> uri()
raw_query(Uri, NewRawQuery) ->
    update_raw(Uri#uri{raw_query = NewRawQuery}).

%% @doc Return the frag field of {@link uri()}.
%% @spec(uri()) -> string()
frag(#uri{frag = Frag}) ->
    Frag.

%% @doc Set the frag field of {@link uri()}.
%% @spec(string()) -> uri()
frag(Uri, NewFrag) ->
    update_raw(Uri#uri{frag = NewFrag}).

%% @doc Return the raw field of {@link uri()}.
%% @spec(uri()) -> string()
raw(#uri{raw = Raw}) ->
    Raw.

update_raw(Uri) ->
    Uri#uri{raw = iolist_to_string(to_iolist(Uri))}.

to_iolist(Uri) ->
    [Uri#uri.scheme, <<"://">>, user_info_to_string(Uri), Uri#uri.host,
     port_to_string(Uri), path_to_string(Uri), raw_query_to_string(Uri),
     frag_to_string(Uri)].
