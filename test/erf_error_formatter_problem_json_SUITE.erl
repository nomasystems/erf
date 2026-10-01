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
-module(erf_error_formatter_problem_json_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        validation_failed,
        unreadable_body,
        route_not_found,
        method_not_allowed
    ].

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
validation_failed(_Conf) ->
    lists:foreach(
        fun({Source, Detail}) ->
            ?assertEqual(
                #{
                    <<"type">> => <<"about:blank">>,
                    <<"title">> => <<"Bad Request">>,
                    <<"status">> => 400,
                    <<"detail">> => Detail
                },
                problem(400, {validation_failed, reason, Source})
            )
        end,
        [
            {body, <<"Request body failed schema validation">>},
            {{path, <<"id">>}, <<"Path parameter \"id\" failed schema validation">>},
            {{query, <<"page">>}, <<"Query parameter \"page\" failed schema validation">>},
            {{header, <<"x-trace">>}, <<"Header parameter \"x-trace\" failed schema validation">>},
            {{cookie, <<"session">>}, <<"Cookie parameter \"session\" failed schema validation">>},
            {undefined, <<"Request failed schema validation">>}
        ]
    ).

unreadable_body(_Conf) ->
    ?assertMatch(
        #{<<"title">> := <<"Bad Request">>, <<"detail">> := <<"Failed to read request">>},
        problem(400, unreadable_body)
    ).

route_not_found(_Conf) ->
    ?assertMatch(
        #{<<"title">> := <<"Not Found">>, <<"detail">> := <<"Route not found">>},
        problem(404, route_not_found)
    ).

method_not_allowed(_Conf) ->
    Methods = [get, post, put, delete, patch, head, options, trace, connect],
    Allow = <<"GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS, TRACE, CONNECT">>,
    Error = {method_not_allowed, Methods},
    ?assertEqual(
        #{
            <<"type">> => <<"about:blank">>,
            <<"title">> => <<"Method Not Allowed">>,
            <<"status">> => 405,
            <<"detail">> => <<"Allowed methods are ", Allow/binary>>
        },
        problem(405, Error)
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
problem(Status, Error) ->
    {Status, Headers, Body} = erf_error_formatter_problem_json:format(Error),
    ?assertEqual(
        <<"application/problem+json">>, proplists:get_value(<<"content-type">>, Headers)
    ),
    #{<<"status">> := Status} = Problem = json:decode(Body),
    Problem.
