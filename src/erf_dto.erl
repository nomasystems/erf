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
-module(erf_dto).

%%% EXTERNAL EXPORTS
-export([
    generate/2,
    load/1
]).

%%% TYPES
-type t() :: ndto:t().
-type schema() :: ndto:schema().

%%% EXPORTED TYPES
-export_type([
    t/0,
    schema/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(Name, Schema) -> Result when
    Name :: atom(),
    Schema :: schema(),
    Result :: t().
%% @doc Generates an Erlang Syntax Tree of a DTO module from a schema.
generate(Name, Schema) ->
    ndto:generate(Name, Schema).

-spec load(DTO) -> Result when
    DTO :: t(),
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
%% @doc Loads a DTO module into the Erlang Runtime System.
load(DTO) ->
    ndto:load(DTO).
