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
-module(erf_util_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        to_pascal_case,
        to_snake_case,
        safe_binary_to_integer,
        binary_to_number
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
to_pascal_case(_Conf) ->
    ?assertEqual(<<"PascalCase">>, erf_util:to_pascal_case(<<"pascal_case">>)),
    ?assertEqual(<<"WithSymbols">>, erf_util:to_pascal_case(<<"with#.$symbols">>)).

to_snake_case(_Conf) ->
    ?assertEqual(<<"snake_case">>, erf_util:to_snake_case(<<"SnakeCase">>)),
    ?assertEqual(<<"with_symbols">>, erf_util:to_snake_case(<<"WITH#.$Symbols">>)).

safe_binary_to_integer(_Conf) ->
    ?assertEqual(1, erf_util:safe_binary_to_integer(<<"1">>)),
    ?assertEqual(-2, erf_util:safe_binary_to_integer(<<"-2">>)),
    ?assertEqual(null, erf_util:safe_binary_to_integer(<<"a">>)),
    ?assertEqual(null, erf_util:safe_binary_to_integer(<<"">>)).

binary_to_number(_Conf) ->
    ?assertEqual(1, erf_util:binary_to_number(<<"1">>)),
    ?assertEqual(0, erf_util:binary_to_number(<<"0">>)),
    ?assertEqual(1.5, erf_util:binary_to_number(<<"1.5">>)),
    ?assertEqual(-0.5, erf_util:binary_to_number(<<"-0.5">>)),
    ?assertEqual(null, erf_util:binary_to_number(<<"a">>)),
    ?assertEqual(null, erf_util:binary_to_number(<<"">>)).
