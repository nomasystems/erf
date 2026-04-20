%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
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
-module(erf_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        foo,
        middlewares,
        statics,
        swagger_ui,
        start_stop,
        reload_conf,
        stream,
        stream_http_1_0,
        stream_with_postprocess_middleware,
        stream_producer_crash,
        stream_non_200_status,
        callback_crash
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

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
foo(_Conf) ->
    meck:new([erf_callback], [non_strict, no_link]),

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            {200, [], <<"bar">>}
        end
    ),
    meck:expect(
        erf_callback,
        create_foo,
        fun(_Request) ->
            {201, [], <<"bar">>}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?string=1", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?page=1", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?page=2.5", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?page=test", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?page=true", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?price=1.25", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?price=2", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?price=test", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?price=true", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?enabled=true", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?enabled=false", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?enabled=2", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?integerArray=1&integerArray=2&integerArray=3", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?integerArray=1&integerArray=2&integerArray=true", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?numberArray=1.5&numberArray=2&numberArray=-0.85", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?numberArray=1&numberArray=2.0&numberArray=true", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders4, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?boolArray=true&boolArray=false&boolArray=true", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo?boolArray=1&boolArray=2&boolArray=true", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}},
        httpc:request(
            post,
            {"http://localhost:8789/1/foo", [], "application/json", <<"\"foobar\"">>},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 201, "Created"}, _Result3Headers, <<"\"bar\"">>}},
        httpc:request(
            post,
            {"http://localhost:8789/1/foo", [], "application/json", <<"\"bar\"">>},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 404, "Not Found"}, _Result3Headers, <<>>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/not_found", []},
            [],
            [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),

    meck:unload(erf_callback),

    ok.

middlewares(_Conf) ->
    meck:new(
        [
            erf_preprocess_middleware,
            erf_preprocess_stop_middleware,
            erf_callback,
            erf_postprocess_middleware
        ],
        [
            non_strict, no_link
        ]
    ),

    meck:expect(
        erf_preprocess_middleware,
        preprocess,
        fun(#{path := [_Version | Path]} = Request) ->
            Request#{path => [<<"1">> | Path]}
        end
    ),
    meck:expect(
        erf_preprocess_stop_middleware,
        preprocess,
        fun
            (#{method := trace} = _Request) ->
                {stop, {403, [], undefined}};
            (Req) ->
                Req
        end
    ),
    meck:expect(
        erf_callback,
        get_foo,
        fun(#{path_parameters := [{<<"version">>, <<"1">>}]} = _Request) ->
            {200, [], <<"bar">>}
        end
    ),
    meck:expect(
        erf_postprocess_middleware,
        postprocess,
        fun(_Req, {StatusCode, Headers, Body}) ->
            {StatusCode, [{<<"Content-Type">>, <<"application/json">>} | Headers], Body}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        preprocess_middlewares => [erf_preprocess_middleware, erf_preprocess_stop_middleware],
        callback => erf_callback,
        postprocess_middlewares => [erf_postprocess_middleware],
        port => 8789,
        name => erf_server
    }),

    {ok, {{"HTTP/1.1", ResultStatusCode, _ResultHTTPBody}, ResultHeaders, ResultBody}} =
        httpc:request(
            get,
            {"http://localhost:8789/2/foo", []},
            [],
            [{body_format, binary}]
        ),
    ?assertEqual(200, ResultStatusCode),
    ?assertEqual(
        true,
        lists:member(
            {"content-type", "application/json"},
            ResultHeaders
        )
    ),
    ?assertEqual(<<"\"bar\"">>, ResultBody),

    {ok, {{"HTTP/1.1", Result2StatusCode, _Result2HTTPBody}, _Result2Headers, _Result2Body}} =
        httpc:request(
            trace,
            {"http://localhost:8789/2/foo", []},
            [],
            [{body_format, binary}]
        ),
    ?assertEqual(403, Result2StatusCode),

    ok = erf:stop(erf_server),

    meck:unload([
        erf_preprocess_middleware,
        erf_preprocess_stop_middleware,
        erf_callback,
        erf_postprocess_middleware
    ]),

    ok.

statics(_Conf) ->
    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        static_routes => [
            {<<"/static">>, {dir, filename:join([code:lib_dir(erf), "test", <<"fixtures">>])}},
            {<<"/common">>,
                {file,
                    filename:join(
                        [code:lib_dir(erf), "test", <<"fixtures/common_oas_3_0_spec.json">>]
                    )}}
        ],
        name => erf_server
    }),

    {ok, Common} = file:read_file(
        filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/common_oas_3_0_spec.json">>]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders, Common}},
        httpc:request(
            get,
            {"http://localhost:8789/common", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 206, "Partial Content"}, _ResultHeaders, <<"{">>}},
        httpc:request(
            get,
            {"http://localhost:8789/static/common_oas_3_0_spec.json", [{"range", "bytes=0-0"}]},
            [],
            [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),

    ok.

swagger_ui(_Conf) ->
    {ok, Petstore} = file:read_file(
        filename:join(
            code:priv_dir(erf), <<"oas/3.0/examples/petstore.json">>
        )
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            code:priv_dir(erf), <<"oas/3.0/examples/petstore.json">>
        ),
        callback => erf_callback,
        port => 8789,
        swagger_ui => true,
        name => erf_server
    }),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders, <<"<!DOCTYPE html", _SwaggerUI/binary>>}},
        httpc:request(
            get,
            {"http://localhost:8789/swagger", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders, Petstore}},
        httpc:request(
            get,
            {"http://localhost:8789/swagger/spec.json", []},
            [],
            [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server).

start_stop(_Conf) ->
    {ok, Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    ?assertNotException(
        exit,
        {_Reason, {sys, get_state, [Pid]}},
        sys:get_state(Pid)
    ),

    ok = erf:stop(erf_server),

    ?assertExit(
        {_Reason, {sys, get_state, [Pid]}},
        sys:get_state(Pid)
    ),

    ok.

reload_conf(_Conf) ->
    meck:new([erf_callback, erf_callback_2], [non_strict, no_link]),

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            {200, [], <<"bar">>}
        end
    ),

    meck:expect(
        erf_callback_2,
        get_foo,
        fun(_Request) ->
            {200, [], <<"baz">>}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        )
    ),

    ok = erf:reload_conf(erf_server, #{callback => erf_callback_2}),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _Result2Headers, <<"\"baz\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        )
    ),

    erf:stop(erf_server),

    meck:unload(erf_callback),
    meck:unload(erf_callback_2),

    ok.

stream(_Conf) ->
    meck:new([erf_callback], [non_strict, no_link]),

    Chunks = [<<"chunk-1">>, <<"chunk-2">>, <<"chunk-3">>],
    Producer =
        fun(Send) ->
            lists:foreach(fun(Chunk) -> ok = Send(Chunk) end, Chunks)
        end,

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            {200, [{<<"content-type">>, <<"text/plain">>}], {stream, Producer}}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}} =
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        ),

    ?assertEqual(iolist_to_binary(Chunks), Body),
    ?assertEqual(
        "text/plain",
        proplists:get_value("content-type", Headers)
    ),

    RawBytes = raw_http_1_1_get(8789, <<"/1/foo">>),
    ?assertMatch({match, _}, re:run(RawBytes, <<"Transfer-Encoding: chunked">>, [caseless])),
    ?assertMatch({match, _}, re:run(RawBytes, <<"\r\n7\r\nchunk-1\r\n">>)),
    ?assertMatch({match, _}, re:run(RawBytes, <<"\r\n7\r\nchunk-2\r\n">>)),
    ?assertMatch({match, _}, re:run(RawBytes, <<"\r\n7\r\nchunk-3\r\n">>)),
    ?assertMatch({match, _}, re:run(RawBytes, <<"\r\n0\r\n\r\n$">>)),

    ok = erf:stop(erf_server),

    meck:unload(erf_callback),

    ok.

stream_http_1_0(_Conf) ->
    meck:new([erf_callback], [non_strict, no_link]),

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            Producer = fun(Send) -> ok = Send(<<"unreachable">>) end,
            {200, [{<<"content-type">>, <<"text/plain">>}], {stream, Producer}}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    {ok, Sock} = gen_tcp:connect(
        "localhost",
        8789,
        [binary, {active, false}, {packet, http_bin}]
    ),
    ok = gen_tcp:send(
        Sock,
        <<"GET /1/foo HTTP/1.0\r\nHost: localhost:8789\r\n\r\n">>
    ),
    {ok, {http_response, _Version, Status, _Reason}} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),

    ?assertEqual(505, Status),

    ok = erf:stop(erf_server),

    meck:unload(erf_callback),

    ok.

stream_with_postprocess_middleware(_Conf) ->
    meck:new(
        [erf_callback, erf_postprocess_middleware],
        [non_strict, no_link]
    ),

    Chunks = [<<"alpha">>, <<"beta">>],
    Producer =
        fun(Send) ->
            lists:foreach(fun(Chunk) -> ok = Send(Chunk) end, Chunks)
        end,

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            {200, [{<<"content-type">>, <<"text/plain">>}], {stream, Producer}}
        end
    ),

    meck:expect(
        erf_postprocess_middleware,
        postprocess,
        fun(_Req, {Status, Headers, Body}) ->
            {Status, [{<<"x-touched-by-middleware">>, <<"yes">>} | Headers], Body}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        postprocess_middlewares => [erf_postprocess_middleware],
        port => 8789,
        name => erf_server
    }),

    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}} =
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        ),

    ?assertEqual(iolist_to_binary(Chunks), Body),
    ?assertEqual(
        "yes",
        proplists:get_value("x-touched-by-middleware", Headers)
    ),

    ok = erf:stop(erf_server),

    meck:unload([erf_callback, erf_postprocess_middleware]),

    ok.

stream_producer_crash(_Conf) ->
    meck:new([erf_callback], [non_strict, no_link]),

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            BoomProducer = fun(_Send) -> erlang:error(boom) end,
            {200, [{<<"content-type">>, <<"text/plain">>}], {stream, BoomProducer}}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body}} =
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        ),
    ?assertEqual(<<>>, Body),

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            {200, [], <<"bar">>}
        end
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),

    meck:unload(erf_callback),

    ok.

stream_non_200_status(_Conf) ->
    meck:new([erf_callback], [non_strict, no_link]),

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            Producer = fun(Send) -> ok = Send(<<"teapot-body">>) end,
            {418, [{<<"content-type">>, <<"text/plain">>}], {stream, Producer}}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body}} =
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        ),
    ?assertEqual(<<"teapot-body">>, Body),

    ok = erf:stop(erf_server),

    meck:unload(erf_callback),

    ok.

callback_crash(_Conf) ->
    meck:new([erf_callback], [non_strict, no_link]),

    meck:expect(
        erf_callback,
        get_foo,
        fun(_Request) ->
            erlang:error(kaboom)
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/with_refs_oas_3_0_spec.json">>]
        ),
        callback => erf_callback,
        port => 8789,
        name => erf_server
    }),

    %% Elli catches the crash and returns a 500. This exercises
    %% handle_event(request_error, ...) and handle_exception/3.
    {ok, {{"HTTP/1.1", Status, _}, _Headers, _Body}} =
        httpc:request(
            get,
            {"http://localhost:8789/1/foo", []},
            [],
            [{body_format, binary}]
        ),
    ?assertEqual(500, Status),

    ok = erf:stop(erf_server),

    meck:unload(erf_callback),

    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL HELPERS
%%%-----------------------------------------------------------------------------
raw_http_1_1_get(Port, Path) ->
    {ok, Sock} = gen_tcp:connect(
        "localhost",
        Port,
        [binary, {active, false}, {packet, raw}]
    ),
    ok = gen_tcp:send(
        Sock,
        <<
            "GET ",
            Path/binary,
            " HTTP/1.1\r\nHost: localhost:",
            (integer_to_binary(Port))/binary,
            "\r\nConnection: close\r\n\r\n"
        >>
    ),
    All = raw_recv(Sock, <<>>),
    gen_tcp:close(Sock),
    All.

raw_recv(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Bytes} -> raw_recv(Sock, <<Acc/binary, Bytes/binary>>);
        {error, closed} -> Acc
    end.
