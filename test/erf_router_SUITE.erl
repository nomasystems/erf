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
            foo
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
                <<"type">> => <<"integer">>,
                <<"nullable">> => false
            },
            <<"get_foo_request_body">> => undefined,
            <<"get_foo_response_body">> => #{
                <<"anyOf">> => [
                    #{
                        <<"anyOf">> => [
                            #{
                                <<"description">> => <<"A foo">>,
                                <<"enum">> => [<<"bar">>, <<"baz">>],
                                <<"type">> => <<"string">>
                            }
                        ]
                    },
                    #{
                        <<"anyOf">> => [
                            #{
                                <<"properties">> => #{
                                    <<"description">> => #{
                                        <<"description">> =>
                                            <<"An English human-friendly description of the error.">>,
                                        <<"type">> => <<"string">>
                                    }
                                },
                                <<"type">> => <<"object">>
                            }
                        ]
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
                        type => path
                    }
                ],
                operations => [
                    #{
                        id => <<"get_foo">>,
                        method => get,
                        parameters => [],
                        request_body => undefined,
                        response_body => <<"get_foo_response_body">>
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

    meck:expect(foo_callback, get_foo, fun(_PathParameters,  _QueryParameters, _Headers, _Body) ->
        {200, [], <<"bar">>}
    end),
    meck:expect(version_foo_version, is_valid, fun(_Value) -> true end),
    meck:expect(get_foo_request_body, is_valid, fun(_Value) -> true end),

    Req = {
        _Path = [<<"1">>, <<"foo">>],
        _Method = get,
        _QueryParameters = [],
        _Headers = [],
        _Body = <<>>
    },

    ?assertEqual({200, [], <<"bar">>}, Mod:handle(Req)),

    meck:expect(version_foo_version, is_valid, fun(_Value) -> false end),

    ?assertEqual({400, [], undefined}, Mod:handle(Req)),

    meck:unload([
        foo_callback,
        version_foo_version,
        get_foo_request_body
    ]),

    ok.
