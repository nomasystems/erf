%%% Copyright 2024 Nomasystems, S.L. http://www.nomasystems.com
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

% @doc Eases debugging the erf-generated modules used for validating request
% bodies.
-module(erf_dbg).

%%% EXTERNAL EXPORTS
-export([
    register/2,
    trace/1,
    untrace/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------

% @doc Registers an operation ID to module association. Doing such registering
% is required for the `trace` method on this module to properly work. The
% provided operation ID needs to be a string obtained by calling
% `erf_util:to_snake_case`. This function is not to be called multiple times
-spec register(SnakeOperationId, Module) -> Result when
    SnakeOperationId :: binary(),
    Module :: module(),
    Result :: ok.
register(SnakeOperationId, Module) ->
    ok = ensure_ets(),
    true = ets:insert(?MODULE, {SnakeOperationId, Module}),
    ok.

% @doc Traces the module associated to the provided `OperationId`. For this to
% work it is required that such association has been registered using the
% `register` method on this module.
-spec trace(OperationId) -> Result when
    OperationId :: string(),
    Result :: not_found | {ok, MatchDesc},
    MatchDesc :: [MatchInfo],
    MatchInfo :: {saved, integer()} | MatchNum,
    MatchNum :: {matched, node(), 1} | {matched, node(), 0, RPCError},
    RPCError :: term().
trace(OperationId) ->
    SnakeOperationId = erf_util:to_snake_case(OperationId),
    case ets:lookup(?MODULE, SnakeOperationId) of
        [{_OperationId, Module}] ->
            dbg:tracer(),
            dbg:p(all, c),
            dbg:tpl(Module, cx);
        [] ->
            not_found
    end.

% @doc Removes traces from the module associated to the provided `OperationId`.
% Always returns `ok`.
-spec untrace(OperationId) -> Result when
    OperationId :: string(),
    Result :: ok.
untrace(OperationId) ->
    SnakeOperationId = erf_util:to_snake_case(OperationId),
    case ets:lookup(?MODULE, SnakeOperationId) of
        [{_OperationId, Module}] ->
            dbg:ctpl(Module, cx);
        [] ->
            ok
    end,
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
ensure_ets() ->
    case ets:whereis(?MODULE) of
        undefined ->
            ?MODULE = ets:new(?MODULE, [public, named_table]),
            ok;
        _ ->
            ok
    end.
