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
%
%% @doc <code>erf</code>'s configuration manager module.
-module(erf_conf).

%%% EXTERNAL EXPORTS
-export([
    clear/1,
    log_level/1,
    get/1,
    preprocess_middlewares/1,
    postprocess_middlewares/1,
    route_patterns/1,
    router/1,
    router_mod/1,
    set/2
]).

%%% TYPES
-type t() :: #{
    callback => module(),
    log_level => logger:level(),
    preprocess_middlewares => [module()],
    postprocess_middlewares => [module()],
    route_patterns => erf:route_patterns(),
    router => erl_syntax:syntaxTree(),
    router_mod => module(),
    spec_path => binary(),
    spec_parser => module(),
    static_routes => [erf:static_route()],
    swagger_ui => boolean()
}.

%%% EXPORT TYPES
-export_type([
    t/0
]).

%%% MACROS
-define(KEY(Name),
    (erlang:binary_to_atom(<<"erf_", (erlang:atom_to_binary(Name))/binary, "_conf">>))
).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec clear(Name) -> Result when
    Name :: atom(),
    Result :: ok.
%% @doc Clears the configuration for the given <code>Name</code>.
clear(Name) ->
    true = persistent_term:erase(?KEY(Name)),
    ok.

-spec get(Name) -> Result when
    Name :: atom(),
    Result :: {ok, Conf} | {error, not_found},
    Conf :: t().
%% @doc Returns the configuration for the given <code>Name</code>.
get(Name) ->
    case persistent_term:get(?KEY(Name), undefined) of
        undefined ->
            {error, not_found};
        Conf ->
            {ok, Conf}
    end.

-spec log_level(Name) -> Result when
    Name :: atom(),
    Result :: {ok, LogLevel} | {error, not_found},
    LogLevel :: logger:level().
%% @doc Returns the log level for the given <code>Name</code>.
log_level(Name) ->
    case ?MODULE:get(Name) of
        {error, not_found} ->
            {error, not_found};
        {ok, Conf} ->
            {ok, maps:get(log_level, Conf)}
    end.

-spec preprocess_middlewares(Name) -> Result when
    Name :: atom(),
    Result :: {ok, PreprocessMiddlewares} | {error, not_found},
    PreprocessMiddlewares :: [module()].
%% @doc Returns the preprocess middlewares for the given <code>Name</code>.
preprocess_middlewares(Name) ->
    case ?MODULE:get(Name) of
        {error, not_found} ->
            {error, not_found};
        {ok, Conf} ->
            {ok, maps:get(preprocess_middlewares, Conf)}
    end.

-spec postprocess_middlewares(Name) -> Result when
    Name :: atom(),
    Result :: {ok, PostprocessMiddlewares} | {error, not_found},
    PostprocessMiddlewares :: [module()].
%% @doc Returns the postprocess middlewares for the given <code>Name</code>.
postprocess_middlewares(Name) ->
    case ?MODULE:get(Name) of
        {error, not_found} ->
            {error, not_found};
        {ok, Conf} ->
            {ok, maps:get(postprocess_middlewares, Conf)}
    end.

-spec route_patterns(Name) -> Result when
    Name :: atom(),
    Result :: {ok, RoutePatterns} | {error, not_found},
    RoutePatterns :: erf:route_patterns().
%% @doc Returns a mapping between the routes and their matching RegEx for a given <code>Name</code>.
route_patterns(Name) ->
    case ?MODULE:get(Name) of
        {error, not_found} ->
            {error, not_found};
        {ok, Conf} ->
            {ok, maps:get(route_patterns, Conf)}
    end.

-spec router(Name) -> Result when
    Name :: atom(),
    Result :: {ok, Router} | {error, not_found},
    Router :: erl_syntax:syntaxTree().
%% @doc Returns the router for the given <code>Name</code>.
router(Name) ->
    case ?MODULE:get(Name) of
        {error, not_found} ->
            {error, not_found};
        {ok, Conf} ->
            {ok, maps:get(router, Conf)}
    end.

-spec router_mod(Name) -> Result when
    Name :: atom(),
    Result :: {ok, RouterMod} | {error, not_found},
    RouterMod :: module().
%% @doc Returns the router module name for the given <code>Name</code>.
router_mod(Name) ->
    case ?MODULE:get(Name) of
        {error, not_found} ->
            {error, not_found};
        {ok, Conf} ->
            {ok, maps:get(router_mod, Conf)}
    end.

-spec set(Name, Conf) -> Result when
    Name :: atom(),
    Conf :: t(),
    Result :: ok.
%% @doc Sets the configuration for the given <code>Name</code>.
set(Name, Conf) ->
    ok = persistent_term:put(?KEY(Name), Conf),
    ok.
