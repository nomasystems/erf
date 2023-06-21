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
        swagger_ui
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
        fun(_PathParameters, _Headers, _QueryParameters, _Body) ->
            {200, [], <<"bar">>}
        end
    ),
    meck:expect(
        erf_callback,
        create_foo,
        fun(_PathParameters, _Headers, _QueryParameters, _Body) ->
            {201, [], <<"bar">>}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            code:lib_dir(erf, test), <<"fixtures/with_refs_oas_3_0_spec.json">>
        ),
        callback => erf_callback,
        port => 8789
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

    meck:unload(erf_callback),

    ok.

middlewares(_Conf) ->
    meck:new([erf_preprocess_middleware, erf_callback, erf_postprocess_middleware], [
        non_strict, no_link
    ]),

    meck:expect(
        erf_preprocess_middleware,
        preprocess,
        fun({[_Version | Path], Method, Headers, QueryParameters, Body}) ->
            {[<<"1">> | Path], Method, Headers, QueryParameters, Body}
        end
    ),
    meck:expect(
        erf_callback,
        get_foo,
        fun([{<<"version">>, <<"1">>}], _Headers, _QueryParameters, _Body) ->
            {200, [], <<"bar">>}
        end
    ),
    meck:expect(
        erf_postprocess_middleware,
        postprocess,
        fun(_Req, {StatusCode, Headers, Body}) ->
            {StatusCode, [{<<"Accept">>, <<"application/json">>} | Headers], Body}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            code:lib_dir(erf, test), <<"fixtures/with_refs_oas_3_0_spec.json">>
        ),
        preprocess_middlewares => [erf_preprocess_middleware],
        callback => erf_callback,
        postprocess_middlewares => [erf_postprocess_middleware],
        port => 8789
    }),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders, <<"\"bar\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/2/foo", []},
            [],
            [{body_format, binary}]
        )
    ),

    meck:unload([erf_preprocess_middleware, erf_callback, erf_postprocess_middleware]),

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
        ]
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
        swagger_ui => true
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
    ).
