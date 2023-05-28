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

%% @doc Module that parses an specification file into an API AST.
-module(erf_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/2
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
    responses => #{
        '*' | inet:status_code() := ref()
    }
}.
-type parameter() :: #{
    ref := ref(),
    name := parameter_name(),
    type := parameter_type()
}.
-type parameter_name() :: binary().
-type parameter_type() :: header | cookie | path | query.
-type path() :: binary().
-type ref() :: binary().
-type schema() :: ndto:schema().
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

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
parser(oas_3_0) ->
    {ok, erf_parser_oas_3_0};
parser(SpecFormat) ->
    {error, {unsupported_spec_format, SpecFormat}}.
