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
-module(erf_router).

%%% EXTERNAL EXPORTS
-export([
    generate/1,
    load/1
]).

%%% TYPES
-type t() :: erl_syntax:syntaxTree().

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(API) -> Router when
    API :: erf:api(),
    Router :: t().
%% @doc Generates an Erlang Syntax Tree of a router module from an API AST.
generate(_API) ->
    erlang:throw({not_implemented, 'generate/1'}).

-spec load(Router) -> Result when
    Router :: t(),
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
%% @doc Loads a router module into the Erlang Runtime System.
load(Router) ->
    ndto:load(Router).
