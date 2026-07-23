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
-module(erf_versioning_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        single_spec_path_is_unaffected,
        multi_version_routing_and_isolated_validation,
        default_version_alias_routes,
        swagger_ui_multi_version,
        per_version_callback_modules
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
single_spec_path_is_unaffected(_Conf) ->
    %% A single, non-versioned `spec_path` (today's only supported shape) must keep behaving
    %% exactly as before: no version prefix in the routes and no `version` key in `Request`.
    meck:new([erf_versioning_callback], [non_strict, no_link]),

    meck:expect(
        erf_versioning_callback,
        list_items,
        fun(Request) ->
            {200, [], erlang:atom_to_binary(maps:is_key(version, Request))}
        end
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => filename:join(
            [code:lib_dir(erf), "test", <<"fixtures/versioning_v1_oas_3_0_spec.json">>]
        ),
        callback => erf_versioning_callback,
        port => 8789,
        name => erf_server
    }),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, <<"\"false\"">>}},
        httpc:request(
            get,
            {"http://localhost:8789/items", []},
            [],
            [{body_format, binary}]
        )
    ),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 404, "Not Found"}, _Headers2, _Body2}},
        httpc:request(
            get,
            {"http://localhost:8789/v1/items", []},
            [],
            [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),
    meck:unload(erf_versioning_callback),
    ok.

multi_version_routing_and_isolated_validation(_Conf) ->
    %% Two versions of the same API, sharing the `listItems` operation (and therefore the same
    %% callback function) while `createItem` diverges: v2 requires an extra `price` field. v2
    %% also adds a `deleteItem` operation that doesn't exist in v1.
    meck:new([erf_versioning_callback], [non_strict, no_link]),

    meck:expect(
        erf_versioning_callback,
        list_items,
        fun(#{version := Version}) -> {200, [], Version} end
    ),
    meck:expect(
        erf_versioning_callback,
        create_item,
        fun(#{body := Body}) -> {201, [], Body} end
    ),
    meck:expect(
        erf_versioning_callback,
        delete_item,
        fun(_Request) -> {204, [], undefined} end
    ),

    V1Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v1_oas_3_0_spec.json">>]
    ),
    V2Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v2_oas_3_0_spec.json">>]
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => #{<<"v1">> => V1Path, <<"v2">> => V2Path},
        callback => erf_versioning_callback,
        port => 8789,
        name => erf_server
    }),

    %% Same operationId, same callback function, discriminated by `version`.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H1, <<"\"v1\"">>}},
        httpc:request(
            get, {"http://localhost:8789/v1/items", []}, [], [{body_format, binary}]
        )
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H2, <<"\"v2\"">>}},
        httpc:request(
            get, {"http://localhost:8789/v2/items", []}, [], [{body_format, binary}]
        )
    ),

    %% A body valid under v1's schema (no `price`) is accepted on v1...
    ?assertMatch(
        {ok, {{"HTTP/1.1", 201, "Created"}, _H3, _Body3}},
        httpc:request(
            post,
            {"http://localhost:8789/v1/items", [], "application/json", <<"{\"name\":\"foo\"}">>},
            [],
            [{body_format, binary}]
        )
    ),

    %% ...but rejected on v2, which requires `price` too: proof that the two versions'
    %% validation DTOs don't collide despite sharing the operationId `createItem`.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, "Bad Request"}, _H4, _Body4}},
        httpc:request(
            post,
            {"http://localhost:8789/v2/items", [], "application/json", <<"{\"name\":\"foo\"}">>},
            [],
            [{body_format, binary}]
        )
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 201, "Created"}, _H5, _Body5}},
        httpc:request(
            post,
            {"http://localhost:8789/v2/items", [], "application/json",
                <<"{\"name\":\"foo\",\"price\":1.5}">>},
            [],
            [{body_format, binary}]
        )
    ),

    %% `deleteItem` only exists in v2.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 204, "No Content"}, _H6, _Body6}},
        httpc:request(
            delete, {"http://localhost:8789/v2/items/abc", []}, [], [{body_format, binary}]
        )
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 404, "Not Found"}, _H7, _Body7}},
        httpc:request(
            delete, {"http://localhost:8789/v1/items/abc", []}, [], [{body_format, binary}]
        )
    ),

    %% No `default_version` configured: the unprefixed route doesn't exist.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 404, "Not Found"}, _H8, _Body8}},
        httpc:request(
            get, {"http://localhost:8789/items", []}, [], [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),
    meck:unload(erf_versioning_callback),
    ok.

default_version_alias_routes(_Conf) ->
    %% With `default_version` set, the unprefixed routes keep working, aliased to that version
    %% -- the mechanism meant to avoid breaking existing clients when a single-spec API adopts
    %% versioning.
    meck:new([erf_versioning_callback], [non_strict, no_link]),

    meck:expect(
        erf_versioning_callback,
        list_items,
        fun(#{version := Version}) -> {200, [], Version} end
    ),

    V1Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v1_oas_3_0_spec.json">>]
    ),
    V2Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v2_oas_3_0_spec.json">>]
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => #{<<"v1">> => V1Path, <<"v2">> => V2Path},
        default_version => <<"v2">>,
        callback => erf_versioning_callback,
        port => 8789,
        name => erf_server
    }),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H1, <<"\"v2\"">>}},
        httpc:request(
            get, {"http://localhost:8789/items", []}, [], [{body_format, binary}]
        )
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H2, <<"\"v2\"">>}},
        httpc:request(
            get, {"http://localhost:8789/v2/items", []}, [], [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),
    meck:unload(erf_versioning_callback),
    ok.

swagger_ui_multi_version(_Conf) ->
    meck:new([erf_versioning_callback], [non_strict, no_link]),

    V1Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v1_oas_3_0_spec.json">>]
    ),
    V2Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v2_oas_3_0_spec.json">>]
    ),

    {ok, V1Spec} = file:read_file(V1Path),
    {ok, V2Spec} = file:read_file(V2Path),

    {ok, _Pid} = erf:start_link(#{
        spec_path => #{<<"v1">> => V1Path, <<"v2">> => V2Path},
        default_version => <<"v2">>,
        callback => erf_versioning_callback,
        port => 8789,
        swagger_ui => true,
        name => erf_server
    }),

    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H1, <<"<!DOCTYPE html", _SwaggerUI/binary>>}},
        httpc:request(
            get, {"http://localhost:8789/swagger", []}, [], [{body_format, binary}]
        )
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H2, V1Spec}},
        httpc:request(
            get, {"http://localhost:8789/swagger/v1/spec.json", []}, [], [{body_format, binary}]
        )
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H3, V2Spec}},
        httpc:request(
            get, {"http://localhost:8789/swagger/v2/spec.json", []}, [], [{body_format, binary}]
        )
    ),
    %% `default_version => <<"v2">>` aliases the bare `/swagger/spec.json` route to v2's spec.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H4, V2Spec}},
        httpc:request(
            get, {"http://localhost:8789/swagger/spec.json", []}, [], [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),
    meck:unload(erf_versioning_callback),
    ok.

per_version_callback_modules(_Conf) ->
    %% `callback` can also be a per-version map, giving each version its own dedicated
    %% controller module instead of sharing one -- useful when versions diverge enough that a
    %% single module branching on `version` would hurt more than it helps.
    meck:new([erf_versioning_callback_v1, erf_versioning_callback_v2], [non_strict, no_link]),

    meck:expect(
        erf_versioning_callback_v1,
        list_items,
        fun(_Request) -> {200, [], <<"from v1 module">>} end
    ),
    meck:expect(
        erf_versioning_callback_v2,
        list_items,
        fun(_Request) -> {200, [], <<"from v2 module">>} end
    ),
    meck:expect(
        erf_versioning_callback_v2,
        delete_item,
        fun(_Request) -> {204, [], undefined} end
    ),

    V1Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v1_oas_3_0_spec.json">>]
    ),
    V2Path = filename:join(
        [code:lib_dir(erf), "test", <<"fixtures/versioning_v2_oas_3_0_spec.json">>]
    ),

    {ok, _Pid} = erf:start_link(#{
        spec_path => #{<<"v1">> => V1Path, <<"v2">> => V2Path},
        callback => #{
            <<"v1">> => erf_versioning_callback_v1,
            <<"v2">> => erf_versioning_callback_v2
        },
        port => 8789,
        name => erf_server
    }),

    %% Same operationId (`listItems`), but each version's route reaches its own module.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H1, <<"\"from v1 module\"">>}},
        httpc:request(
            get, {"http://localhost:8789/v1/items", []}, [], [{body_format, binary}]
        )
    ),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _H2, <<"\"from v2 module\"">>}},
        httpc:request(
            get, {"http://localhost:8789/v2/items", []}, [], [{body_format, binary}]
        )
    ),

    %% `deleteItem` only exists in v2's spec, so only the v2 module needs to implement it.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 204, "No Content"}, _H3, _Body3}},
        httpc:request(
            delete, {"http://localhost:8789/v2/items/abc", []}, [], [{body_format, binary}]
        )
    ),

    ok = erf:stop(erf_server),
    meck:unload(erf_versioning_callback_v1),
    meck:unload(erf_versioning_callback_v2),
    ok.
