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

%% @doc <code>erf</code> is a library that provides a design-first framework to build
%% RESTful APIs in Erlang.
-module(erf).

%%% BEHAVIOURS
-behaviour(supervisor).

%%% INCLUDE FILES
-include_lib("kernel/include/logger.hrl").

%%% START/STOP EXPORTS
-export([
    start_link/1,
    stop/1
]).

%%% EXTERNAL EXPORTS
-export([
    get_router/1,
    reload_conf/2
]).

%%% INIT/TERMINATE EXPORTS
-export([
    init/1
]).

%%% TYPES
-type api() :: erf_parser:api().
-type body() :: njson:t().
-type conf() :: #{
    spec_path := binary(),
    spec_parser => module(),
    callback := module(),
    preprocess_middlewares => [module()],
    postprocess_middlewares => [module()],
    port => inet:port_number(),
    ssl => boolean(),
    certfile => binary(),
    keyfile => binary(),
    static_routes => [static_route()],
    swagger_ui => boolean(),
    name => atom(),
    min_acceptors => pos_integer(),
    accept_timeout => pos_integer(),
    request_timeout => pos_integer(),
    header_timeout => pos_integer(),
    body_timeout => pos_integer(),
    max_body_size => pos_integer(),
    log_level => logger:level()
}.
-type header() :: {binary(), binary()}.
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
-type path_parameter() :: {binary(), binary()}.
-type query_parameter() :: {binary(), binary()}.
-type request() :: {
    Path :: [binary()],
    Method :: method(),
    QueryParameters :: [query_parameter()],
    Headers :: [header()],
    Body :: body()
}.
-type response() :: {
    StatusCode :: pos_integer(),
    Headers :: [header()],
    Body :: body() | {file, binary()}
}.
-type static_dir() :: {dir, binary()}.
-type static_file() :: {file, binary()}.
-type static_route() :: {Path :: binary(), Resource :: static_file() | static_dir()}.

%%% TYPE EXPORTS
-export_type([
    api/0,
    body/0,
    conf/0,
    header/0,
    method/0,
    path_parameter/0,
    query_parameter/0,
    request/0,
    response/0,
    static_route/0
]).

%%% MACROS
-define(ELLI_SERVER_NAME(Name),
    (erlang:binary_to_atom(<<"erf_", (erlang:atom_to_binary(Name))/binary>>))
).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(Conf) -> Result when
    Conf :: conf(),
    Result :: {ok, Pid} | ignore | {error, Reason},
    Pid :: pid(),
    Reason :: term().
%% @doc Starts the supervision tree for an instance of the server.
start_link(Conf) ->
    Name = maps:get(name, Conf, erf),
    supervisor:start_link(
        {local, Name},
        ?MODULE,
        [Name, Conf]
    ).

-spec stop(Name) -> Result when
    Name :: atom(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Stops the supervision tree for an instance of the server.
stop(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, server_not_started};
        Pid ->
            true = erlang:exit(Pid, normal),
            erf_conf:clear(Name),
            ok
    end.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec get_router(Name) -> Result when
    Name :: atom(),
    Result :: {ok, Router} | {error, Reason},
    Router :: binary(),
    Reason :: term().
%% @doc Returns the router for an instance of the server.
get_router(Name) ->
    case erf_conf:router(Name) of
        {ok, RawRouter} ->
            case unicode:characters_to_binary(erl_prettypr:format(RawRouter)) of
                {error, _Bin, _RestData} ->
                    {error, cannot_format_router};
                {incomplete, _Bin, _RestData} ->
                    {error, cannot_format_router};
                Router ->
                    {ok, Router}
            end;
        {error, not_found} ->
            {error, server_not_started}
    end.

-spec reload_conf(Name, Conf) -> Result when
    Name :: atom(),
    Conf :: erf_conf:t(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Reloads the configuration for an instance of the server.
reload_conf(Name, NewConf) ->
    OldConf =
        case erf_conf:get(Name) of
            {error, not_found} ->
                #{};
            {ok, Old} ->
                Old
        end,

    Conf = maps:merge(OldConf, NewConf),

    SpecPath = maps:get(spec_path, Conf),
    SpecParser = maps:get(spec_parser, Conf),
    Callback = maps:get(callback, Conf),
    StaticRoutes = maps:get(static_routes, Conf),
    SwaggerUI = maps:get(swagger_ui, Conf),

    case build_router(SpecPath, SpecParser, Callback, StaticRoutes, SwaggerUI) of
        {ok, RouterMod, Router} ->
            erf_conf:set(Name, Conf#{router_mod => RouterMod, router => Router}),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([Name, RawConf]) ->
    RawErfConf = #{
        spec_path => maps:get(spec_path, RawConf),
        spec_parser => maps:get(spec_parser, RawConf, erf_oas_3_0),
        callback => maps:get(callback, RawConf),
        static_routes => maps:get(static_routes, RawConf, []),
        swagger_ui => maps:get(swagger_ui, RawConf, false),
        preprocess_middlewares => maps:get(preprocess_middlewares, RawConf, []),
        postprocess_middlewares => maps:get(postprocess_middlewares, RawConf, []),
        log_level => maps:get(log_level, RawConf, error)
    },

    SpecPath = maps:get(spec_path, RawErfConf),
    SpecParser = maps:get(spec_parser, RawErfConf),
    Callback = maps:get(callback, RawErfConf),
    StaticRoutes = maps:get(static_routes, RawErfConf),
    SwaggerUI = maps:get(swagger_ui, RawErfConf),

    case build_router(SpecPath, SpecParser, Callback, StaticRoutes, SwaggerUI) of
        {ok, RouterMod, Router} ->
            ErfConf = RawErfConf#{router_mod => RouterMod, router => Router},
            ok = erf_conf:set(Name, ErfConf),

            ElliConf = build_elli_conf(RawConf),
            SupFlags = #{
                strategy => one_for_one,
                intensity => 1,
                period => 5
            },
            ChildSpec = {
                Name,
                {elli, start_link, [ElliConf]},
                permanent,
                5000,
                worker,
                [elli]
            },
            {ok, {SupFlags, [ChildSpec]}};
        {error, Reason} ->
            {stop, Reason}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec build_dtos(Schemas) -> Result when
    Schemas :: [{erf_parser:ref(), ndto:schema()}],
    Result :: ok | {error, Reason},
    Reason :: term().
build_dtos([]) ->
    ok;
build_dtos([{Ref, Schema} | Schemas]) ->
    Name = erlang:binary_to_atom(Ref),
    DTO = ndto:generate(Name, Schema),
    case ndto:load(DTO) of
        ok ->
            build_dtos(Schemas);
        {ok, Warnings} ->
            log_warnings(Warnings, <<"dtos generation">>),
            build_dtos(Schemas);
        error ->
            {error, {dto_loading_failed, [unknown_error]}};
        {error, {Errors, Warnings}} ->
            log_warnings(Warnings, <<"dtos generation">>),
            {error, {dto_loading_failed, Errors}}
    end.

-spec build_elli_conf(Conf) -> ElliConf when
    Conf :: conf(),
    ElliConf :: [{atom(), term()}].
build_elli_conf(Conf) ->
    Name = maps:get(name, Conf, erf),
    lists:filter(
        fun
            ({_K, undefined}) -> false;
            ({_K, _V}) -> true
        end,
        [
            {callback, erf_router},
            {callback_args, [Name]},
            {port, maps:get(port, Conf, 8080)},
            {ssl, maps:get(ssl, Conf, false)},
            {certfile, maps:get(certfile, Conf, undefined)},
            {keyfile, maps:get(keyfile, Conf, undefined)},
            {name, {local, ?ELLI_SERVER_NAME(Name)}},
            {min_acceptors, maps:get(min_acceptors, Conf, undefined)},
            {accept_timeout, maps:get(accept_timeout, Conf, undefined)},
            {request_timeout, maps:get(request_timeout, Conf, undefined)},
            {header_timeout, maps:get(header_timeout, Conf, undefined)},
            {body_timeout, maps:get(body_timeout, Conf, undefined)},
            {max_body_size, maps:get(max_body_size, Conf, undefined)}
        ]
    ).

-spec build_router(SpecPath, SpecParser, Callback, StaticRoutes, SwaggerUI) -> Result when
    SpecPath :: binary(),
    SpecParser :: module(),
    Callback :: module(),
    StaticRoutes :: [static_route()],
    SwaggerUI :: boolean(),
    Result :: {ok, RouterMod, Router} | {error, Reason},
    RouterMod :: module(),
    Router :: erl_syntax:syntaxTree(),
    Reason :: term().
build_router(SpecPath, SpecParser, Callback, RawStaticRoutes, SwaggerUI) ->
    case erf_parser:parse(SpecPath, SpecParser) of
        {ok, API} ->
            Schemas = maps:to_list(maps:get(schemas, API)),
            case build_dtos(Schemas) of
                ok ->
                    StaticRoutes =
                        case SwaggerUI of
                            true ->
                                IndexHTML =
                                    case code:priv_dir(erf) of
                                        {error, bad_name} ->
                                            {error, <<"Cannot build `swagger-ui`">>};
                                        Priv ->
                                            filename:join([Priv, <<"swagger-ui">>, <<"index.html">>])
                                    end,
                                [
                                    {<<"/swagger">>, {file, IndexHTML}},
                                    {<<"/swagger/spec.json">>, {file, SpecPath}}
                                    | RawStaticRoutes
                                ];
                            _False ->
                                RawStaticRoutes
                        end,
                    {RouterMod, Router} = erf_router:generate(API, #{
                        callback => Callback,
                        static_routes => StaticRoutes
                    }),
                    case erf_router:load(Router) of
                        ok ->
                            {ok, RouterMod, Router};
                        {ok, Warnings} ->
                            log_warnings(Warnings, <<"router generation">>),
                            {ok, RouterMod, Router};
                        error ->
                            {error, {router_loading_failed, [unknown_error]}};
                        {error, {Errors, Warnings}} ->
                            log_warnings(Warnings, <<"router generation">>),
                            {error, {router_loading_failed, Errors}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec log_warnings(Warnings, Step) -> ok when
    Warnings :: list(),
    Step :: binary().
log_warnings(Warnings, Step) ->
    lists:foreach(
        fun(Warning) ->
            ?LOG_WARNING("[erf] Warning found during ~p: ~p~n", [Step, Warning])
        end,
        Warnings
    ).
