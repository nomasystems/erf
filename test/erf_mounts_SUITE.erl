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
-module(erf_mounts_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(PORT, 8789).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        single_spec_path_is_unaffected,
        mounts_route_by_base_path,
        isolated_validation_across_mounts,
        swagger_ui_per_mount,
        invalid_conf,
        reload_conf_replaces_mounts
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
    meck:new([erf_items_callback], [non_strict, no_link]),
    meck:expect(erf_items_callback, list_items, fun(_Request) -> {200, [], <<"items">>} end),

    {ok, _Pid} = erf:start_link(#{
        spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
        callback => erf_items_callback,
        port => ?PORT,
        name => erf_server
    }),

    ?assertMatch({200, <<"\"items\"">>}, http_get("/items")),
    ?assertMatch({404, _Body}, http_get("/v1/items")),
    ?assertMatch({ok, <<"/items">>}, erf:match_route(erf_server, <<"/items">>)),

    ok = erf:reload_conf(erf_server, #{log_level => warning}),
    ?assertMatch({200, <<"\"items\"">>}, http_get("/items")),

    ok = erf:stop(erf_server),
    meck:unload(erf_items_callback),
    ok.

mounts_route_by_base_path(_Conf) ->
    meck:new([erf_items_callback, erf_items_v1_callback, erf_orders_callback], [
        non_strict, no_link
    ]),
    meck:expect(erf_items_callback, list_items, fun(_Request) -> {200, [], <<"root">>} end),
    meck:expect(erf_items_v1_callback, list_items, fun(_Request) -> {200, [], <<"v1">>} end),
    meck:expect(erf_orders_callback, list_orders, fun(#{path := Path}) ->
        {200, [], erlang:iolist_to_binary(lists:join(<<"/">>, Path))}
    end),
    meck:expect(erf_orders_callback, get_order, fun(#{path_parameters := PathParameters}) ->
        {200, [], proplists:get_value(<<"id">>, PathParameters)}
    end),

    {ok, _Pid} = erf:start_link(#{
        mounts => [
            #{
                base_path => <<>>,
                spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
                callback => erf_items_callback
            },
            #{
                base_path => <<"/v1">>,
                spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
                callback => erf_items_v1_callback
            },
            #{
                base_path => <<"/shop">>,
                spec_path => spec(<<"mount_orders_oas_3_0_spec.json">>),
                callback => erf_orders_callback
            }
        ],
        port => ?PORT,
        name => erf_server
    }),

    ?assertMatch({200, <<"\"root\"">>}, http_get("/items")),
    ?assertMatch({200, <<"\"v1\"">>}, http_get("/v1/items")),
    ?assertMatch({200, <<"\"shop/orders\"">>}, http_get("/shop/orders")),
    ?assertMatch({200, <<"\"42\"">>}, http_get("/shop/orders/42")),

    ?assertMatch({404, _Body}, http_get("/orders")),
    ?assertMatch({404, _Body2}, http_get("/v2/items")),

    ?assertMatch(
        {ok, <<"/shop/orders/{id}">>}, erf:match_route(erf_server, <<"/shop/orders/42">>)
    ),

    ok = erf:stop(erf_server),
    meck:unload([erf_items_callback, erf_items_v1_callback, erf_orders_callback]),
    ok.

isolated_validation_across_mounts(_Conf) ->
    %% Both fixtures are named `catalog_oas_3_0_spec.json', so the parser generates the same
    %% schema names for the two mounts.
    meck:new([erf_catalog_a_callback, erf_catalog_b_callback], [non_strict, no_link]),
    meck:expect(erf_catalog_a_callback, create_entry, fun(_Request) -> {201, [], <<"a">>} end),
    meck:expect(erf_catalog_b_callback, create_entry, fun(_Request) -> {201, [], <<"b">>} end),

    {ok, _Pid} = erf:start_link(#{
        mounts => [
            #{
                base_path => <<"/a">>,
                spec_path => spec(<<"mount_a/catalog_oas_3_0_spec.json">>),
                callback => erf_catalog_a_callback
            },
            #{
                base_path => <<"/b">>,
                spec_path => spec(<<"mount_b/catalog_oas_3_0_spec.json">>),
                callback => erf_catalog_b_callback
            }
        ],
        port => ?PORT,
        name => erf_server
    }),

    NamedEntry = <<"{\"name\":\"an entry\"}">>,
    ReferencedEntry = <<"{\"reference\":\"an entry\"}">>,

    ?assertMatch({201, <<"\"a\"">>}, post("/a/entries", NamedEntry)),
    ?assertMatch({400, _Body}, post("/a/entries", ReferencedEntry)),

    ?assertMatch({201, <<"\"b\"">>}, post("/b/entries", ReferencedEntry)),
    ?assertMatch({400, _Body2}, post("/b/entries", NamedEntry)),

    ok = erf:stop(erf_server),
    meck:unload([erf_catalog_a_callback, erf_catalog_b_callback]),
    ok.

swagger_ui_per_mount(_Conf) ->
    meck:new([erf_items_callback, erf_orders_callback], [non_strict, no_link]),

    {ok, _Pid} = erf:start_link(#{
        mounts => [
            #{
                base_path => <<>>,
                spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
                callback => erf_items_callback
            },
            #{
                base_path => <<"/shop">>,
                spec_path => spec(<<"mount_orders_oas_3_0_spec.json">>),
                callback => erf_orders_callback
            }
        ],
        swagger_ui => true,
        port => ?PORT,
        name => erf_server
    }),

    ?assertMatch({200, _IndexHTML}, http_get("/swagger")),
    ?assertMatch({200, _ShopIndexHTML}, http_get("/shop/swagger")),

    {200, RootSpec} = http_get("/swagger/spec.json"),
    ?assertMatch(#{<<"paths">> := #{<<"/items">> := _Items}}, json:decode(RootSpec)),

    {200, ShopSpec} = http_get("/shop/swagger/spec.json"),
    ?assertMatch(#{<<"paths">> := #{<<"/orders">> := _Orders}}, json:decode(ShopSpec)),

    ok = erf:stop(erf_server),
    meck:unload([erf_items_callback, erf_orders_callback]),
    ok.

invalid_conf(_Conf) ->
    meck:new([erf_items_callback], [non_strict, no_link]),

    {ok, _Pid} = erf:start_link(#{
        spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
        callback => erf_items_callback,
        port => ?PORT,
        name => erf_server
    }),

    ItemsMount = #{
        base_path => <<"/v1">>,
        spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
        callback => erf_items_callback
    },

    ?assertEqual(
        {error, {duplicate_base_path, <<"/v1">>}},
        erf:reload_conf(erf_server, #{mounts => [ItemsMount, ItemsMount#{callback => other}]})
    ),

    %% The specifications share a route once mounted: `/orders/items' against `/orders/{id}'.
    ?assertEqual(
        {error, {conflicting_routes, <<"/orders/items">>, <<"/orders/{id}">>}},
        erf:reload_conf(erf_server, #{
            mounts => [
                ItemsMount#{base_path => <<"/orders">>},
                #{
                    base_path => <<>>,
                    spec_path => spec(<<"mount_orders_oas_3_0_spec.json">>),
                    callback => erf_items_callback
                }
            ]
        })
    ),

    ?assertEqual(
        {error, {invalid_base_path, <<"/{tenant}">>}},
        erf:reload_conf(erf_server, #{mounts => [ItemsMount#{base_path => <<"/{tenant}">>}]})
    ),

    ?assertEqual(
        {error, {invalid_conf, missing_mounts}},
        erf:reload_conf(erf_server, #{mounts => []})
    ),

    ?assertEqual(
        {error, {invalid_conf, missing_spec_path}},
        erf:reload_conf(erf_server, #{callback => erf_items_callback})
    ),

    ?assertEqual(
        {error, {invalid_conf, missing_callback}},
        erf:reload_conf(erf_server, #{spec_path => spec(<<"mount_items_oas_3_0_spec.json">>)})
    ),

    ?assertMatch({ok, <<"/items">>}, erf:match_route(erf_server, <<"/items">>)),

    ok = erf:stop(erf_server),
    meck:unload(erf_items_callback),
    ok.

reload_conf_replaces_mounts(_Conf) ->
    meck:new([erf_items_callback, erf_orders_callback], [non_strict, no_link]),
    meck:expect(erf_items_callback, list_items, fun(_Request) -> {200, [], <<"items">>} end),
    meck:expect(erf_orders_callback, list_orders, fun(_Request) -> {200, [], <<"orders">>} end),

    {ok, _Pid} = erf:start_link(#{
        mounts => [
            #{
                base_path => <<"/v1">>,
                spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
                callback => erf_items_callback
            }
        ],
        port => ?PORT,
        name => erf_server
    }),

    ?assertMatch({200, <<"\"items\"">>}, http_get("/v1/items")),
    ?assertMatch({404, _Body}, http_get("/shop/orders")),

    ok = erf:reload_conf(erf_server, #{
        mounts => [
            #{
                base_path => <<"/shop">>,
                spec_path => spec(<<"mount_orders_oas_3_0_spec.json">>),
                callback => erf_orders_callback
            }
        ]
    }),

    ?assertMatch({200, <<"\"orders\"">>}, http_get("/shop/orders")),
    ?assertMatch({404, _Body2}, http_get("/v1/items")),

    ok = erf:reload_conf(erf_server, #{log_level => warning}),
    ?assertMatch({200, <<"\"orders\"">>}, http_get("/shop/orders")),

    ok = erf:reload_conf(erf_server, #{
        spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
        callback => erf_items_callback
    }),
    ?assertMatch({200, <<"\"items\"">>}, http_get("/items")),
    ?assertMatch({404, _Body3}, http_get("/shop/orders")),

    ok = erf:reload_conf(erf_server, #{
        mounts => [
            #{
                base_path => <<"/shop">>,
                spec_path => spec(<<"mount_orders_oas_3_0_spec.json">>),
                callback => erf_orders_callback
            }
        ],
        spec_path => spec(<<"mount_items_oas_3_0_spec.json">>),
        callback => erf_items_callback
    }),
    ?assertMatch({200, <<"\"orders\"">>}, http_get("/shop/orders")),
    ?assertMatch({404, _Body4}, http_get("/items")),

    ok = erf:stop(erf_server),
    meck:unload([erf_items_callback, erf_orders_callback]),
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
spec(Fixture) ->
    filename:join([code:lib_dir(erf), "test", <<"fixtures/", Fixture/binary>>]).

url(Path) ->
    "http://localhost:" ++ erlang:integer_to_list(?PORT) ++ Path.

http_get(Path) ->
    {ok, {{_HTTPVersion, Status, _Reason}, _Headers, Body}} = httpc:request(
        get, {url(Path), []}, [], [{body_format, binary}]
    ),
    {Status, Body}.

post(Path, Body) ->
    {ok, {{_HTTPVersion, Status, _Reason}, _Headers, ResponseBody}} = httpc:request(
        post,
        {url(Path), [], "application/json", Body},
        [],
        [{body_format, binary}]
    ),
    {Status, ResponseBody}.
