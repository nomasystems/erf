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
-module(erf_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/1,
    parse/2,
    to_snake_case/1
]).

%%% TYPES
-type api() :: #{
    name := binary(),
    version := binary(),
    endpoints := [endpoint()],
    schemas := #{ref() => schema()}
}.
-type endpoint() :: #{
    path := path(),
    parameters := [parameter()],
    operations := [operation()]
}.
-type method() ::
    get
    | post
    | put
    | delete
    | patch
    | head
    | options
    | trace
    | connect.
-type operation() :: #{
    id := binary(),
    method := method(),
    parameters := [parameter()],
    request_body => ref(),
    response_body => ref()
}.
-type parameter() :: #{
    ref => ref(),
    name := parameter_name(),
    type := parameter_type()
}.
-type parameter_name() :: binary().
-type parameter_type() :: header | cookie | path | query.
-type path() :: binary().
-type ref() :: binary().
-type schema() :: erf_dto:schema().
-type spec_format() :: oas_3_0.

%%% TYPE EXPORTS
-export_type([
    api/0,
    endpoint/0,
    method/0,
    operation/0,
    parameter/0,
    ref/0,
    schema/0,
    spec_format/0
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback parse(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, API} | {error, Reason},
    API :: api(),
    Reason :: term().

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, API} | {error, Reason},
    API :: api(),
    Reason :: term().
%% @equiv parse(SpecPath, oas_3_0).
parse(SpecPath) ->
    parse(SpecPath, oas_3_0).

-spec parse(SpecPath, SpecFormat) -> Result when
    SpecPath :: binary(),
    SpecFormat :: spec_format(),
    Result :: {ok, API} | {error, Reason},
    API :: api(),
    Reason :: term().
%% @doc Parses an specification file into an API AST given a specification format.
parse(SpecPath, SpecFormat) ->
    case parser(SpecFormat) of
        {ok, Parser} ->
            Parser:parse(SpecPath);
        {error, Reason} ->
            {error, Reason}
    end.

-spec to_snake_case(Binary) -> SnakeCase when
    Binary :: binary(),
    SnakeCase :: binary().
%% @doc Naive function to convert a string to snake_case.
to_snake_case(Binary) ->
    to_snake_case(erlang:binary_to_list(Binary), []).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
parser(oas_3_0) ->
    {ok, erf_parser_oas_3_0};
parser(SpecFormat) ->
    {error, {unsupported_spec_format, SpecFormat}}.

to_snake_case([], Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc));
to_snake_case([C1, C2 | Rest], Acc) when
    ((C1 >= $a andalso C1 =< $z) orelse (C1 >= $0 andalso C1 =< $9)) andalso
        C2 >= $A andalso C2 =< $Z
->
    to_snake_case(Rest, [C2 + 32, $_, C1 | Acc]);
to_snake_case([C | Rest], Acc) when (C >= $a andalso C =< $z); (C >= $0 andalso C =< $9) ->
    to_snake_case(Rest, [C | Acc]);
to_snake_case([C | Rest], Acc) when C >= $A andalso C =< $Z ->
    to_snake_case(Rest, [C + 32 | Acc]);
to_snake_case([_C | Rest], Acc) ->
    to_snake_case(Rest, [$_ | Acc]).
