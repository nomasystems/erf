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
            validation_sources,
            method_not_allowed_allow
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

    {Mod, Router} = erf_router:generate(API, #{
        callback => foo_callback, error_formatter => erf_error_formatter_problem_json
    }),
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

    meck:expect(get_foo_request_body, is_valid, fun(_Value) -> {false, reason} end),

    ?assertMatch(
        {400, [{<<"content-type">>, <<"application/problem+json">>}], _Problem},
        Mod:handle(Req)
    ),

    NotAllowedReq = #{
        path => [<<"1">>, <<"foo">>],
        method => post,
        query_parameters => [],
        headers => [],
        body => <<>>,
        peer => <<"localhost">>
    },

    ?assertMatch(
        {405,
            [
                {<<"content-type">>, <<"application/problem+json">>},
                {<<"allow">>, <<"GET">>}
            ],
            _MethodNotAllowed},
        Mod:handle(NotAllowedReq)
    ),

    meck:unload([
        foo_callback,
        version_foo_version,
        get_foo_request_body
    ]),

    ok.

validation_sources(_Conf) ->
    API = #{
        name => <<"Sources">>,
        version => <<"1.0.0">>,
        schemas => #{},
        endpoints => [
            #{
                path => <<"/{tenant}/bar">>,
                parameters => [
                    #{
                        ref => <<"sources_tenant">>,
                        name => <<"tenant">>,
                        type => path,
                        required => true
                    },
                    #{
                        ref => <<"sources_page">>,
                        name => <<"page">>,
                        type => query,
                        required => true
                    },
                    #{
                        ref => <<"sources_session">>,
                        name => <<"session">>,
                        type => cookie,
                        required => true
                    },
                    #{
                        ref => <<"sources_trace">>,
                        name => <<"x-trace">>,
                        type => header,
                        required => true
                    }
                ],
                operations => [
                    #{
                        id => <<"get_bar">>,
                        method => get,
                        parameters => [],
                        request => #{
                            body => #{ref => <<"sources_body">>, required => false}
                        },
                        responses => #{}
                    }
                ]
            }
        ]
    },

    {Mod, Router} = erf_router:generate(API, #{
        callback => sources_callback, error_formatter => sources_error_formatter
    }),
    ok = erf_router:load(Router),

    Validators = [sources_tenant, sources_page, sources_trace, sources_body],
    meck:new([sources_callback, sources_error_formatter | Validators], [non_strict, no_link]),
    meck:expect(sources_callback, get_bar, fun(_Request) -> {200, [], <<"bar">>} end),
    meck:expect(
        sources_error_formatter,
        format,
        fun({validation_failed, _Reason, Source}) -> {400, [], Source} end
    ),

    Req = #{
        path => [<<"acme">>, <<"bar">>],
        method => get,
        query_parameters => [{<<"page">>, <<"1">>}],
        headers => [{<<"x-trace">>, <<"t">>}],
        body => <<"{}">>,
        peer => <<"localhost">>
    },

    Pass = fun() ->
        lists:foreach(fun(V) -> meck:expect(V, is_valid, fun(_) -> true end) end, Validators)
    end,

    Pass(),
    ?assertEqual({200, [], <<"bar">>}, Mod:handle(Req)),

    lists:foreach(
        fun({Validator, Source}) ->
            Pass(),
            meck:expect(Validator, is_valid, fun(_Value) -> {false, reason} end),
            ?assertEqual({400, [], Source}, Mod:handle(Req))
        end,
        [
            {sources_body, body},
            {sources_tenant, {path, <<"tenant">>}},
            {sources_page, {query, <<"page">>}},
            {sources_trace, {header, <<"x-trace">>}}
        ]
    ),

    meck:expect(sources_error_formatter, format, fun(_Error) -> erlang:error(oops) end),
    ?assertEqual({400, [], undefined}, Mod:handle(Req)),
    ?assertEqual(
        {400, [], undefined},
        erf_router:error_response(sources_error_formatter, unreadable_body)
    ),

    lists:foreach(
        fun(InvalidResponse) ->
            meck:expect(sources_error_formatter, format, fun(_Error) -> InvalidResponse end),
            ?assertEqual({400, [], undefined}, Mod:handle(Req)),
            ?assertEqual(
                {400, [], undefined},
                erf_router:error_response(sources_error_formatter, unreadable_body)
            )
        end,
        [
            ok,
            {400, []},
            {999, [], undefined},
            {400, not_a_list, undefined},
            {400, [{"content-type", "text/plain"}], undefined}
        ]
    ),

    meck:unload([sources_callback, sources_error_formatter | Validators]),

    ok.

method_not_allowed_allow(_Conf) ->
    API = #{
        name => <<"Allow">>,
        version => <<"1.0.0">>,
        schemas => #{},
        endpoints => [
            #{
                path => <<"/baz">>,
                parameters => [],
                operations => [
                    #{
                        id => Id,
                        method => Method,
                        parameters => [],
                        request => #{body => #{ref => <<"allow_body">>, required => false}},
                        responses => #{}
                    }
                 || {Id, Method} <- [{<<"get_baz">>, get}, {<<"delete_baz">>, delete}]
                ]
            }
        ]
    },
    Req = #{
        path => [<<"baz">>],
        method => post,
        query_parameters => [],
        headers => [],
        body => <<>>,
        peer => <<"localhost">>
    },
    Allow = {<<"allow">>, <<"GET, DELETE">>},

    lists:foreach(
        fun(ErrorFormatter) ->
            {Mod, Router} = erf_router:generate(API, #{
                callback => allow_callback, error_formatter => ErrorFormatter
            }),
            ok = erf_router:load(Router),
            ?assertEqual({405, [Allow], undefined}, Mod:handle(Req))
        end,
        [undefined, false]
    ),

    Error = {method_not_allowed, [get, delete]},
    meck:new([allow_error_formatter], [non_strict, no_link]),
    lists:foreach(
        fun({FormatterResponse, Expected}) ->
            meck:expect(allow_error_formatter, format, fun(_Error) -> FormatterResponse end),
            ?assertEqual(Expected, erf_router:error_response(allow_error_formatter, Error))
        end,
        [
            {default, {405, [Allow], undefined}},
            {ok, {405, [Allow], undefined}},
            {{405, [], <<"nope">>}, {405, [Allow], <<"nope">>}},
            {
                {405, [{<<"Allow">>, <<"GET">>}], <<"nope">>},
                {405, [{<<"Allow">>, <<"GET">>}], <<"nope">>}
            },
            {{404, [], <<"hidden">>}, {404, [], <<"hidden">>}}
        ]
    ),
    meck:unload(allow_error_formatter),

    ok.
