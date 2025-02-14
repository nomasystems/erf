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
        reload_conf
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
            code:lib_dir(erf, test), <<"fixtures/with_refs_oas_3_0_spec.json">>
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
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<_/binary>>}},
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
            code:lib_dir(erf, test), <<"fixtures/with_refs_oas_3_0_spec.json">>
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
            code:lib_dir(erf, test), <<"fixtures/with_refs_oas_3_0_spec.json">>
        ),
        callback => erf_callback,
        port => 8789,
        static_routes => [
            {<<"/static">>, {dir, filename:join(code:lib_dir(erf, test), <<"fixtures">>)}},
            {<<"/common">>,
                {file,
                    filename:join(
                        code:lib_dir(erf, test), <<"fixtures/common_oas_3_0_spec.json">>
                    )}}
        ],
        name => erf_server
    }),

    {ok, Common} = file:read_file(
        filename:join(
            code:lib_dir(erf, test), <<"fixtures/common_oas_3_0_spec.json">>
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
            code:lib_dir(erf, test), <<"fixtures/with_refs_oas_3_0_spec.json">>
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
            code:lib_dir(erf, test), <<"fixtures/with_refs_oas_3_0_spec.json">>
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
