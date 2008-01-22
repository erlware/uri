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

-export([from_string/1]).
-export([query_foldl/3]).
-export([query_to_dict/1, query_to_tl/1]).
-export([quote/1, quote/2]).
-export([unquote/1]).

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
%%        <dt>raw_query::string()</dt>
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
%%        <dt>full::string()</dt>
%%        <dd>This is the original uri that the above fields were populated
%%            from. Everything will still be in their original quoted form.
%%            Note that this may be a best guess as to the uri a user had
%%            in their browser, as this will most likely be formed by
%%            concatenating the `Host' header with the `request' line uri.</dd>
%%       </dl>


%% @doc Populate a new #uri record by parsing the string `Uri'
%% @spec from_string(string()) -> uri()
from_string(Uri) ->
    {Scheme, Uri1} = parse_scheme(Uri),

    {Authority, Uri2} = parse_authority(Uri1),
    {UserInfo, HostPort} = parse_user_info(Authority),
    {Host, Port} = parse_host_port(HostPort),

    {Path, Uri3} = parse_path(Uri2),
    {Query, Uri4} = parse_query(Uri3),
    {Frag, Uri5} = parse_frag(Uri4),
    case Uri5 of
        [] -> ok;
        _ -> throw({uri_error, {data_left_after_parsing, Uri5}})
    end,
    #uri{scheme = Scheme,
         user_info = unquote(UserInfo),
         host = Host,
         port = Port,
         path = unquote(Path),
         raw_query = Query,
         frag = unquote(Frag),
         raw=Uri}.

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
    Frag;
parse_frag("") ->
    "".

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
%%       them as some kind of a list
%% @spec query_to_dict(Query) -> dict()
%%       Query = string() | uri()
query_to_dict(Query) ->
    query_foldl(fun ({K, V}, D) -> dict:store(K, V, D) end, dict:new(), Query).

%% @doc Convert the string or the `raw_query' portion of {@link uri()} into
%%      a tuple-list. See {@link query_to_dict/1} for more information, this
%%      is basically the same except that for duplicate keys,
%%      {@link query_to_dict/1} will currently overwrite earlier values where
%%      this will return a tuple-list with multiple key entries.
%% @see query_to_dict/1
%% @spec query_to_tl(Query) -> dict()
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
%% @spec query_foldl(F::function(), term(), Query) -> term()
%%       Query = string() | uri()
query_foldl(F, Init, Query) when is_binary(Query) ->
    query_foldl(F, Init, binary_to_list(Query));
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
                end, Init, string:tokens(Query, "&")).

%% @doc Return `Str' with all `+' replaced with space, and `%NN' replaced
%%      with the decoded byte.
%% @spec unquote(string()) -> string()
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
%% @spec quote(string()) -> string()
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
%% @spec quote(string(), atom()) -> string()
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

