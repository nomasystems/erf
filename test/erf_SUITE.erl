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

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        foo
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
        fun(_Req, _Args) ->
            {200, [], <<"bar">>}
        end
    ),
    meck:expect(
        erf_callback,
        create_foo,
        fun(_Req, _Args) ->
            {201, [], <<"bar">>}
        end
    ),
    meck:expect(
        erf_callback,
        handle_event,
        fun(_Event, _Data, _Args) -> ok end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => erlang:list_to_binary(
            code:lib_dir(erf, test) ++ "/fixtures/with_refs_oas_3_0_spec.json"
        ),
        callback => erf_callback,
        port => 8789
    }),

    {ok, {{"HTTP/1.1", 200, "OK"}, _ResultHeaders, <<"bar">>}} = httpc:request(
        get,
        {"http://localhost:8789/1/foo", []},
        [],
        [{body_format, binary}]
    ),

    {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Result2Headers, <<>>}} = httpc:request(
        post,
        {"http://localhost:8789/1/foo", [], "application/json", <<"foobar">>},
        [],
        [{body_format, binary}]
    ),
    {ok, {{"HTTP/1.1", 201, "Created"}, _Result3Headers, <<"bar">>}} = httpc:request(
        post,
        {"http://localhost:8789/1/foo", [], "application/json", <<"bar">>},
        [],
        [{body_format, binary}]
    ),

    ok.
