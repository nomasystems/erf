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
%% limitations under the License

%% @private
-module(erf_parser_oas_3_0).

%%% BEHAVIOURS
-behaviour(erf_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, API} | {error, Reason},
    API :: erf:api(),
    Reason :: term().
%% @doc Parses an OpenAPI Specification 3.0 file into an API AST.
parse(SpecPath) ->
    case file:read_file(SpecPath) of
        {ok, Bin} ->
            Map = njson:decode(Bin),
            case parse_api(Map) of
                {error, Reason} ->
                    {error, Reason};
                API ->
                    {ok, API}
            end;
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
parse_api(Val) ->
    case oas_3_0:is_valid(Val) of
        true ->
            parse_api(Val, #{});
        false ->
            {error, {invalid_spec, <<"Invalid OpenAPI Specification 3.0">>}}
    end.

parse_api(_Val, _Acc) ->
    %% TODO: implement
    #{}.
