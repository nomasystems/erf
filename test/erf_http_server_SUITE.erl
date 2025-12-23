%%% Copyright 2024 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @doc Test suite for erf HTTP servers (elli and cowboy).
%%
%% This suite runs the same tests against both backends to ensure
%% consistent behavior.
-module(erf_http_server_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(PORT, 8791).
-define(HOST, "localhost").

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        {group, elli},
        {group, cowboy}
    ].

groups() ->
    Tests = [
        basic_get,
        basic_post,
        query_parameters,
        invalid_query_parameters,
        middlewares,
        stop_middleware,
        static_file,
        static_dir,
        range_request,
        swagger_ui,
        not_found
    ],
    [
        {elli, [], Tests},
        {cowboy, [], Tests}
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT GROUP EXPORTS
%%%-----------------------------------------------------------------------------
init_per_group(elli, Conf) ->
    [{http_server, {erf_http_server_elli, #{}}} | Conf];
init_per_group(cowboy, Conf) ->
    [{http_server, {erf_http_server_cowboy, #{}}} | Conf].

%%%-----------------------------------------------------------------------------
%%% END GROUP EXPORTS
%%%-----------------------------------------------------------------------------
end_per_group(_Group, Conf) ->
    Conf.

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
basic_get(Conf) ->
    HttpServer = ?config(http_server, Conf),
    {ok, _Pid} = start_server(HttpServer, #{}),

    {200, _, Body} = request(get, "/1/foo", [], HttpServer),
    ?assertEqual(<<"\"bar\"">>, Body),

    ok = erf:stop(erf_test_server).

basic_post(Conf) ->
    HttpServer = ?config(http_server, Conf),
    {ok, _Pid} = start_server(HttpServer, #{}),

    {201, _, Body} = request(post, "/1/foo", [], <<"\"bar\"">>, <<"application/json">>, HttpServer),
    ?assertEqual(<<"\"bar\"">>, Body),

    ok = erf:stop(erf_test_server).

query_parameters(Conf) ->
    HttpServer = ?config(http_server, Conf),
    {ok, _Pid} = start_server(HttpServer, #{}),

    % Valid string
    {200, _, _} = request(get, "/1/foo?string=test", [], HttpServer),
    % Valid integer
    {200, _, _} = request(get, "/1/foo?page=1", [], HttpServer),
    % Valid number
    {200, _, _} = request(get, "/1/foo?price=1.25", [], HttpServer),
    % Valid boolean
    {200, _, _} = request(get, "/1/foo?enabled=true", [], HttpServer),
    % Valid array
    {200, _, _} = request(get, "/1/foo?integerArray=1&integerArray=2", [], HttpServer),

    ok = erf:stop(erf_test_server).

invalid_query_parameters(Conf) ->
    HttpServer = ?config(http_server, Conf),
    {ok, _Pid} = start_server(HttpServer, #{}),

    % Invalid integer (float)
    {400, _, _} = request(get, "/1/foo?page=2.5", [], HttpServer),
    % Invalid integer (string)
    {400, _, _} = request(get, "/1/foo?page=test", [], HttpServer),
    % Invalid boolean
    {400, _, _} = request(get, "/1/foo?enabled=2", [], HttpServer),
    % Invalid array element
    {400, _, _} = request(get, "/1/foo?integerArray=1&integerArray=true", [], HttpServer),

    ok = erf:stop(erf_test_server).

middlewares(Conf) ->
    HttpServer = ?config(http_server, Conf),
    {ok, _Pid} = start_server(HttpServer, #{
        preprocess_middlewares => [erf_test_middleware],
        postprocess_middlewares => [erf_test_middleware]
    }),

    % Middleware rewrites /2/foo to /1/foo and adds content-type
    {200, Headers, Body} = request(get, "/2/foo", [], HttpServer),
    ?assertEqual(<<"\"bar\"">>, Body),
    ?assertNotEqual(undefined, get_header(<<"content-type">>, Headers)),

    ok = erf:stop(erf_test_server).

stop_middleware(Conf) ->
    HttpServer = ?config(http_server, Conf),
    {ok, _Pid} = start_server(HttpServer, #{
        preprocess_middlewares => [erf_test_stop_middleware]
    }),

    % DELETE should be stopped with 403
    {403, _, _} = request(delete, "/1/foo", [], HttpServer),

    ok = erf:stop(erf_test_server).

static_file(Conf) ->
    HttpServer = ?config(http_server, Conf),
    FilePath = list_to_binary(
        filename:join([code:lib_dir(erf), "test", "fixtures", "common_oas_3_0_spec.json"])
    ),
    {ok, Expected} = file:read_file(FilePath),

    {ok, _Pid} = start_server(HttpServer, #{
        static_routes => [{<<"/common">>, {file, FilePath}}]
    }),

    {200, _, Body} = request(get, "/common", [], HttpServer),
    ?assertEqual(Expected, Body),

    ok = erf:stop(erf_test_server).

static_dir(Conf) ->
    HttpServer = ?config(http_server, Conf),
    DirPath = list_to_binary(filename:join([code:lib_dir(erf), "test", "fixtures"])),
    FilePath = filename:join(DirPath, "common_oas_3_0_spec.json"),
    {ok, Expected} = file:read_file(FilePath),

    {ok, _Pid} = start_server(HttpServer, #{
        static_routes => [{<<"/static">>, {dir, DirPath}}]
    }),

    {200, _, Body} = request(get, "/static/common_oas_3_0_spec.json", [], HttpServer),
    ?assertEqual(Expected, Body),

    ok = erf:stop(erf_test_server).

range_request(Conf) ->
    HttpServer = ?config(http_server, Conf),
    DirPath = list_to_binary(filename:join([code:lib_dir(erf), "test", "fixtures"])),

    {ok, _Pid} = start_server(HttpServer, #{
        static_routes => [{<<"/static">>, {dir, DirPath}}]
    }),

    % First byte only
    {206, _, Body1} = request(
        get, "/static/common_oas_3_0_spec.json", [{<<"range">>, <<"bytes=0-0">>}], HttpServer
    ),
    ?assertEqual(<<"{">>, Body1),

    % First 10 bytes
    {206, Headers, Body2} = request(
        get, "/static/common_oas_3_0_spec.json", [{<<"range">>, <<"bytes=0-9">>}], HttpServer
    ),
    ?assertEqual(10, byte_size(Body2)),
    ?assertNotEqual(undefined, get_header(<<"content-range">>, Headers)),

    ok = erf:stop(erf_test_server).

swagger_ui(Conf) ->
    HttpServer = ?config(http_server, Conf),
    SpecPath = list_to_binary(
        filename:join([code:priv_dir(erf), "oas", "3.0", "examples", "petstore.json"])
    ),
    {ok, Expected} = file:read_file(SpecPath),

    {ok, _Pid} = erf:start_link(#{
        spec_path => SpecPath,
        callback => erf_test_callback,
        port => ?PORT,
        name => erf_test_server,
        swagger_ui => true,
        http_server => HttpServer
    }),

    {200, _, SwaggerBody} = request(get, "/swagger", [], HttpServer),
    ?assertMatch(<<"<!DOCTYPE html", _/binary>>, SwaggerBody),

    {200, _, SpecBody} = request(get, "/swagger/spec.json", [], HttpServer),
    ?assertEqual(Expected, SpecBody),

    ok = erf:stop(erf_test_server).

not_found(Conf) ->
    HttpServer = ?config(http_server, Conf),
    {ok, _Pid} = start_server(HttpServer, #{}),

    {404, _, _} = request(get, "/not/found", [], HttpServer),

    ok = erf:stop(erf_test_server).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
start_server(HttpServer, ExtraOpts) ->
    BaseOpts = #{
        spec_path => spec_path(),
        callback => erf_test_callback,
        port => ?PORT,
        name => erf_test_server,
        http_server => HttpServer
    },
    erf:start_link(maps:merge(BaseOpts, ExtraOpts)).

spec_path() ->
    list_to_binary(
        filename:join([code:lib_dir(erf), "test", "fixtures", "with_refs_oas_3_0_spec.json"])
    ).

request(Method, Path, Headers, HttpServer) ->
    request(Method, Path, Headers, <<>>, undefined, HttpServer).

request(Method, Path, Headers, Body, ContentType, {erf_http_server_cowboy, _}) ->
    http2_request(Method, Path, Headers, Body, ContentType);
request(Method, Path, Headers, Body, ContentType, {erf_http_server_elli, _}) ->
    http1_request(Method, Path, Headers, Body, ContentType).

%% HTTP/1.1 request using httpc
http1_request(Method, Path, Headers, Body, ContentType) ->
    Url = "http://" ++ ?HOST ++ ":" ++ integer_to_list(?PORT) ++ Path,
    HttpcHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    Request =
        case {Method, ContentType} of
            {get, _} -> {Url, HttpcHeaders};
            {delete, _} -> {Url, HttpcHeaders};
            {head, _} -> {Url, HttpcHeaders};
            {options, _} -> {Url, HttpcHeaders};
            {_, CT} -> {Url, HttpcHeaders, binary_to_list(CT), Body}
        end,
    case httpc:request(Method, Request, [], [{body_format, binary}]) of
        {ok, {{_, Status, _}, RespHeaders, RespBody}} ->
            NormHeaders = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- RespHeaders],
            {Status, NormHeaders, RespBody}
    end.

%% HTTP/2 request using gun
http2_request(Method, Path, Headers, Body, ContentType) ->
    {ok, ConnPid} = gun:open(?HOST, ?PORT, #{
        protocols => [http2],
        http2_opts => #{preface_timeout => 10000}
    }),
    {ok, http2} = gun:await_up(ConnPid, 5000),

    Headers1 =
        case ContentType of
            undefined -> Headers;
            CT -> [{<<"content-type">>, CT} | Headers]
        end,

    StreamRef =
        case Method of
            get -> gun:get(ConnPid, Path, Headers1);
            post -> gun:post(ConnPid, Path, Headers1, Body);
            put -> gun:put(ConnPid, Path, Headers1, Body);
            delete -> gun:delete(ConnPid, Path, Headers1);
            head -> gun:head(ConnPid, Path, Headers1);
            options -> gun:options(ConnPid, Path, Headers1);
            patch -> gun:patch(ConnPid, Path, Headers1, Body)
        end,

    Result =
        case gun:await(ConnPid, StreamRef, 5000) of
            {response, fin, Status, RespHeaders} ->
                {Status, RespHeaders, <<>>};
            {response, nofin, Status, RespHeaders} ->
                {ok, RespBody} = gun:await_body(ConnPid, StreamRef, 5000),
                {Status, RespHeaders, RespBody}
        end,

    gun:close(ConnPid),
    Result.

get_header(Name, Headers) ->
    LowerName = string:lowercase(Name),
    case lists:keyfind(LowerName, 1, [{string:lowercase(K), V} || {K, V} <- Headers]) of
        {_, Value} -> Value;
        false -> undefined
    end.
