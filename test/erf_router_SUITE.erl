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
-module(erf_router_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        {group, route}
    ].

groups() ->
    [
        {route, [parallel], [
            foo,
            method_not_allowed
        ]}
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
    API = #{
        name => <<"Foo">>,
        version => <<"1.0.0">>,
        schemas => #{
            <<"version_foo_version">> => #{
                type => integer
            },
            <<"get_foo_request_body">> => true,
            <<"get_foo_response_body_200">> => #{
                any_of => [#{enum => [<<"bar">>, <<"baz">>]}]
            },
            <<"get_foo_response_body_default">> => #{
                any_of => [
                    #{
                        type => object,
                        properties => #{
                            description => #{
                                description =>
                                    <<"An English human-friendly description of the error.">>,
                                type => string
                            }
                        }
                    }
                ]
            }
        },
        endpoints => [
            #{
                path => <<"/{version}/foo">>,
                parameters => [
                    #{
                        ref => <<"version_foo_version">>,
                        name => <<"version">>,
                        type => path,
                        required => true
                    }
                ],
                operations => [
                    #{
                        id => <<"get_foo">>,
                        method => get,
                        parameters => [],
                        request => #{
                            body => #{
                                ref => <<"get_foo_request_body">>,
                                required => false
                            }
                        },
                        responses => #{
                            200 => #{
                                body => #{
                                    ref => <<"get_foo_response_body_200">>
                                }
                            },
                            '*' => #{
                                body => #{
                                    ref => <<"get_foo_response_body_default">>
                                }
                            }
                        }
                    }
                ]
            }
        ]
    },

    {Mod, Router} = erf_router:generate(API, #{callback => foo_callback}),
    ok = erf_router:load(Router),

    meck:new(
        [
            foo_callback,
            version_foo_version,
            get_foo_request_body
        ],
        [
            non_strict,
            no_link
        ]
    ),

    meck:expect(foo_callback, get_foo, fun(_Request) ->
        {200, [], <<"bar">>}
    end),
    meck:expect(version_foo_version, is_valid, fun(_Value) -> true end),
    meck:expect(get_foo_request_body, is_valid, fun(_Value) -> true end),

    Req = #{
        path => [<<"1">>, <<"foo">>],
        method => get,
        query_parameters => [],
        headers => [],
        body => <<>>,
        peer => <<"localhost">>
    },

    ?assertEqual({200, [], <<"bar">>}, Mod:handle(Req)),

    meck:expect(get_foo_request_body, is_valid, fun(_Value) ->
        {false, {'$.type', <<"Value is not an object">>}}
    end),

    {400, BadRequestHeaders, BadRequestBody} = Mod:handle(Req),

    ?assertEqual(
        <<"application/problem+json">>,
        proplists:get_value(<<"content-type">>, BadRequestHeaders)
    ),
    ?assertMatch(
        #{
            <<"status">> := 400,
            <<"title">> := <<"Bad Request">>,
            <<"detail">> := <<"Request body failed schema validation">>,
            <<"errors">> := [
                #{
                    <<"in">> := <<"body">>,
                    <<"pointer">> := <<>>,
                    <<"keyword">> := <<"type">>,
                    <<"detail">> := <<"Value is not an object">>
                }
            ]
        },
        json:decode(BadRequestBody)
    ),

    meck:expect(version_foo_version, is_valid, fun(_Value) ->
        {false, {'$.pattern', <<"String does not match pattern ^[0-9]+$">>}}
    end),
    meck:expect(get_foo_request_body, is_valid, fun(_Value) -> true end),

    {400, _PathHeaders, PathBody} = Mod:handle(Req),

    ?assertMatch(
        #{
            <<"detail">> := <<"Path parameter \"version\" failed schema validation">>,
            <<"errors">> := [
                #{
                    <<"in">> := <<"path">>,
                    <<"pointer">> := <<"/version">>,
                    <<"keyword">> := <<"pattern">>
                }
            ]
        },
        json:decode(PathBody)
    ),

    meck:expect(version_foo_version, is_valid, fun(_Value) -> true end),

    NotAllowedReq = #{
        path => [<<"1">>, <<"foo">>],
        method => post,
        query_parameters => [],
        headers => [],
        body => <<>>,
        peer => <<"localhost">>
    },

    {405, NotAllowedHeaders, NotAllowedBody} = Mod:handle(NotAllowedReq),

    ?assertEqual(<<"GET">>, proplists:get_value(<<"allow">>, NotAllowedHeaders)),
    ?assertEqual(
        <<"application/problem+json">>,
        proplists:get_value(<<"content-type">>, NotAllowedHeaders)
    ),
    ?assertMatch(
        #{
            <<"type">> := <<"about:blank">>,
            <<"title">> := <<"Method Not Allowed">>,
            <<"status">> := 405,
            <<"detail">> :=
                <<
                    "The target resource does not support the request method. "
                    "Supported methods: GET."
                >>
        },
        json:decode(NotAllowedBody)
    ),

    meck:unload([
        foo_callback,
        version_foo_version,
        get_foo_request_body
    ]),

    ok.

method_not_allowed(_Conf) ->
    API = #{
        name => <<"Things">>,
        version => <<"1.0.0">>,
        schemas => #{},
        endpoints => [
            #{
                path => <<"/things">>,
                parameters => [],
                operations => [
                    thing_operation(<<"delete_things">>, delete),
                    thing_operation(<<"get_things">>, get),
                    thing_operation(<<"post_things">>, post)
                ]
            },
            #{
                path => <<"/things/health">>,
                parameters => [],
                operations => [
                    thing_operation(<<"get_things_health">>, get)
                ]
            }
        ]
    },

    {Mod, Router} = erf_router:generate(API, #{callback => things_callback}),
    ok = erf_router:load(Router),

    {405, Headers, Body} = Mod:handle(request([<<"things">>], put)),

    ?assertEqual(<<"GET, POST, DELETE">>, proplists:get_value(<<"allow">>, Headers)),
    ?assertMatch(
        #{
            <<"status">> := 405,
            <<"detail">> :=
                <<
                    "The target resource does not support the request method. "
                    "Supported methods: GET, POST, DELETE."
                >>
        },
        json:decode(Body)
    ),

    {405, HeadHeaders, _HeadBody} = Mod:handle(request([<<"things">>], head)),

    ?assertEqual(<<"GET, POST, DELETE">>, proplists:get_value(<<"allow">>, HeadHeaders)),

    {405, HealthHeaders, _HealthBody} = Mod:handle(request([<<"things">>, <<"health">>], options)),

    ?assertEqual(<<"GET">>, proplists:get_value(<<"allow">>, HealthHeaders)),

    ok.

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------
thing_operation(Id, Method) ->
    #{
        id => Id,
        method => Method,
        parameters => [],
        request => #{
            body => #{
                ref => <<Id/binary, "_request_body">>,
                required => false
            }
        },
        responses => #{}
    }.

request(Path, Method) ->
    #{
        path => Path,
        method => Method,
        query_parameters => [],
        headers => [],
        body => undefined,
        peer => <<"localhost">>
    }.
