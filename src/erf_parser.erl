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

%% @doc Module that parses a specification file into an API AST.
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
-type body() :: #{
    ref := ref(),
    required := boolean()
}.
-type endpoint() :: #{
    path := path(),
    parameters := [parameter()],
    operations := [operation()]
}.
-type method() :: erf:method().
-type operation() :: #{
    id := binary(),
    method := method(),
    parameters := [parameter()],
    request := request(),
    responses := #{
        '*' | status_code() := response()
    }
}.
-type parameter() :: #{
    ref := ref(),
    name := parameter_name(),
    type := parameter_type(),
    required := boolean(),
    schema_type := schema_type()
}.
-type parameter_name() :: binary().
-type parameter_type() :: header | cookie | path | query.
-type schema_type() :: binary().
-type path() :: binary().
-type ref() :: binary().
-type request() :: #{
    body := body()
}.
-type response() :: #{
    body := body()
}.
-type schema() :: ndto:schema().
-type status_code() :: 100..599.

%%% TYPE EXPORTS
-export_type([
    api/0,
    body/0,
    endpoint/0,
    method/0,
    operation/0,
    parameter/0,
    parameter_name/0,
    parameter_type/0,
    path/0,
    ref/0,
    request/0,
    response/0,
    schema/0,
    status_code/0
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
-spec parse(SpecPath, SpecParser) -> Result when
    SpecPath :: binary(),
    SpecParser :: module(),
    Result :: {ok, API} | {error, Reason},
    API :: api(),
    Reason :: term().
%% @doc Parses an specification file into an API AST given a specification format.
parse(SpecPath, SpecParser) ->
    SpecParser:parse(SpecPath).
