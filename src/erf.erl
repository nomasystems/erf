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
    match_route/2,
    reload_conf/2
]).

%%% INIT/TERMINATE EXPORTS
-export([
    init/1
]).

%%% TYPES
-type api() :: erf_parser:api().
-type base_path() :: binary().
-type body() :: undefined | json:decode_value().
-type conf() :: #{
    spec_path => binary(),
    callback => module(),
    mounts => [mount()],
    port => inet:port_number(),
    name => atom(),
    spec_parser => module(),
    preprocess_middlewares => [module()],
    postprocess_middlewares => [module()],
    ssl => boolean(),
    certfile => binary(),
    keyfile => binary(),
    static_routes => [static_route()],
    swagger_ui => boolean(),
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
-type mount() :: #{
    base_path := base_path(),
    spec_path := binary(),
    callback := module(),
    spec_parser => module()
}.
-type path_parameter() :: {binary(), binary()}.
-type query_parameter() :: {binary(), binary()}.
-type request() :: #{
    scheme := undefined | binary(),
    host := undefined | binary(),
    port := undefined | 1..65535,
    path := [binary()],
    path_parameters => [path_parameter()],
    method := method(),
    query_parameters := [query_parameter()],
    headers := [header()],
    body := body(),
    peer := undefined | binary(),
    route := binary(),
    context => any()
}.
-type response() :: {
    StatusCode :: pos_integer(),
    Headers :: [header()],
    Body :: body() | {file, binary()} | stream_body()
}.
-type route_patterns() :: [{Route :: binary(), RouteRegEx :: binary()}].
-type send_chunk_fun() :: fun((iodata()) -> ok | {error, closed | timeout}).
-type static_dir() :: {dir, binary()}.
-type static_file() :: {file, binary()}.
-type static_route() :: {Path :: binary(), Resource :: static_file() | static_dir()}.
-type stream_body() :: {stream, stream_producer()}.
-type stream_producer() :: fun((send_chunk_fun()) -> any()).

%%% TYPE EXPORTS
-export_type([
    api/0,
    base_path/0,
    body/0,
    conf/0,
    header/0,
    method/0,
    mount/0,
    path_parameter/0,
    query_parameter/0,
    request/0,
    response/0,
    route_patterns/0,
    send_chunk_fun/0,
    static_route/0,
    stream_body/0,
    stream_producer/0
]).

%%% MACROS
-define(URL_ENCODED_STRING_REGEX, <<"(?:[^%]|%[0-9A-Fa-f]{2})+">>).
% from https://rgxdb.com/r/48L3HPJP

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

-spec match_route(Name, RawPath) -> Result when
    Name :: atom(),
    RawPath :: binary(),
    Result :: {ok, Route} | {error, Reason},
    Route :: binary(),
    Reason :: term().
match_route(Name, RawPath) ->
    case erf_conf:route_patterns(Name) of
        {ok, RoutePatterns} ->
            match_route_(RawPath, RoutePatterns);
        Error ->
            Error
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

    case build_router(Conf) of
        {ok, Extras} ->
            erf_conf:set(Name, maps:merge(Conf, Extras)),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([Name, RawConf]) ->
    RawErfConf = #{
        spec_path => maps:get(spec_path, RawConf, undefined),
        spec_parser => maps:get(spec_parser, RawConf, erf_parser_oas_3_0),
        callback => maps:get(callback, RawConf, undefined),
        mounts => maps:get(mounts, RawConf, []),
        static_routes => maps:get(static_routes, RawConf, []),
        swagger_ui => maps:get(swagger_ui, RawConf, false),
        preprocess_middlewares => maps:get(preprocess_middlewares, RawConf, []),
        postprocess_middlewares => maps:get(postprocess_middlewares, RawConf, []),
        log_level => maps:get(log_level, RawConf, error)
    },

    case build_router(RawErfConf) of
        {ok, Extras} ->
            ErfConf = maps:merge(RawErfConf, Extras),
            ok = erf_conf:set(Name, ErfConf),

            {HTTPServer, HTTPServerExtraConf} = maps:get(
                http_server, RawConf, {erf_http_server_elli, #{}}
            ),
            HTTPServerConf = build_http_server_conf(RawConf),
            SupFlags = #{
                strategy => one_for_one,
                intensity => 1,
                period => 5
            },
            ChildSpec = {
                Name,
                {erf_http_server, start_link, [
                    HTTPServer, HTTPServerExtraConf, Name, HTTPServerConf
                ]},
                permanent,
                5000,
                worker,
                [erf_http_server]
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

-spec build_http_server_conf(ErfConf) -> HTTPServerConf when
    ErfConf :: erf:conf(),
    HTTPServerConf :: erf_http_server:conf().
build_http_server_conf(ErfConf) ->
    #{
        port => maps:get(port, ErfConf, 8080),
        ssl => maps:get(ssl, ErfConf, false),
        certfile => maps:get(certfile, ErfConf, undefined),
        keyfile => maps:get(keyfile, ErfConf, undefined)
    }.

-spec build_router(Conf) -> Result when
    Conf :: erf_conf:t(),
    Result :: {ok, Extras} | {error, Reason},
    Extras :: #{
        route_patterns := route_patterns(),
        router_mod := module(),
        router := erl_syntax:syntaxTree()
    },
    Reason :: term().
build_router(Conf) ->
    case mounts(Conf) of
        {ok, Mounts} ->
            case swagger_routes(Mounts, maps:get(swagger_ui, Conf)) of
                {ok, SwaggerRoutes} ->
                    build_router(Mounts, SwaggerRoutes ++ maps:get(static_routes, Conf));
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec build_router(Mounts, StaticRoutes) -> Result when
    Mounts :: [mount()],
    StaticRoutes :: [static_route()],
    Result :: {ok, Extras} | {error, Reason},
    Extras :: #{
        route_patterns := route_patterns(),
        router_mod := module(),
        router := erl_syntax:syntaxTree()
    },
    Reason :: term().
build_router(Mounts, StaticRoutes) ->
    case parse_api(Mounts) of
        {ok, API} ->
            Schemas = maps:to_list(maps:get(schemas, API)),
            case build_dtos(Schemas) of
                ok ->
                    {RouterMod, Router} = erf_router:generate(API, #{
                        callback => callbacks(Mounts),
                        static_routes => StaticRoutes
                    }),
                    Extras = #{
                        route_patterns => route_patterns(API, StaticRoutes),
                        router_mod => RouterMod,
                        router => Router
                    },
                    case erf_router:load(Router) of
                        ok ->
                            {ok, Extras};
                        {ok, Warnings} ->
                            log_warnings(Warnings, <<"router generation">>),
                            {ok, Extras};
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

-spec mounts(Conf) -> Result when
    Conf :: erf_conf:t(),
    Result :: {ok, Mounts} | {error, Reason},
    Mounts :: [mount()],
    Reason :: term().
mounts(#{mounts := [_ | _] = RawMounts} = Conf) ->
    Mounts = [normalize_mount(RawMount, Conf) || RawMount <- RawMounts],
    BasePaths = [BasePath || #{base_path := BasePath} <- Mounts],
    InvalidBasePaths = [
        BasePath
     || BasePath <- BasePaths, binary:match(BasePath, <<"{">>) =/= nomatch
    ],
    case {InvalidBasePaths, BasePaths -- lists:uniq(BasePaths)} of
        {[BasePath | _Rest], _DuplicatedBasePaths} ->
            {error, {invalid_base_path, BasePath}};
        {[], [BasePath | _Rest]} ->
            {error, {duplicate_base_path, BasePath}};
        {[], []} ->
            {ok, Mounts}
    end;
mounts(#{spec_path := SpecPath, callback := Callback} = Conf) when
    SpecPath =/= undefined, Callback =/= undefined
->
    Mount = #{base_path => <<"/">>, spec_path => SpecPath, callback => Callback},
    {ok, [normalize_mount(Mount, Conf)]};
mounts(_Conf) ->
    {error, {invalid_conf, missing_spec_path}}.

-spec normalize_mount(Mount, Conf) -> NormalizedMount when
    Mount :: mount(),
    Conf :: erf_conf:t(),
    NormalizedMount :: mount().
normalize_mount(#{base_path := BasePath} = Mount, Conf) ->
    DefaultSpecParser = maps:get(spec_parser, Conf, erf_parser_oas_3_0),
    Mount#{
        base_path => erlang:iolist_to_binary([
            [<<"/">>, Segment]
         || Segment <- path_segments(BasePath)
        ]),
        spec_parser => maps:get(spec_parser, Mount, DefaultSpecParser)
    }.

-spec callbacks(Mounts) -> Callbacks when
    Mounts :: [mount()],
    Callbacks :: #{base_path() => module()}.
callbacks(Mounts) ->
    maps:from_list([
        {BasePath, Callback}
     || #{base_path := BasePath, callback := Callback} <- Mounts
    ]).

-spec parse_api(Mounts) -> Result when
    Mounts :: [mount()],
    Result :: {ok, API} | {error, Reason},
    API :: api(),
    Reason :: term().
parse_api(Mounts) ->
    case parse_mounts(Mounts, []) of
        {ok, [FirstAPI | _Rest] = APIs} ->
            case conflicting_routes(APIs) of
                {ok, Path, OtherPath} ->
                    {error, {conflicting_routes, Path, OtherPath}};
                none ->
                    {ok, FirstAPI#{
                        endpoints => lists:append([maps:get(endpoints, API) || API <- APIs]),
                        schemas => lists:foldl(
                            fun(API, Acc) -> maps:merge(Acc, maps:get(schemas, API)) end,
                            #{},
                            APIs
                        )
                    }}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_mounts(Mounts, APIs) -> Result when
    Mounts :: [mount()],
    APIs :: [api()],
    Result :: {ok, [api()]} | {error, Reason},
    Reason :: term().
parse_mounts([], APIs) ->
    {ok, lists:reverse(APIs)};
parse_mounts([Mount | Mounts], APIs) ->
    #{spec_path := SpecPath, spec_parser := SpecParser} = Mount,
    case erf_parser:parse(SpecPath, SpecParser) of
        {ok, API} ->
            parse_mounts(Mounts, [mount_api(Mount, API) | APIs]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec mount_api(Mount, API) -> MountedAPI when
    Mount :: mount(),
    API :: api(),
    MountedAPI :: api().
mount_api(#{base_path := BasePath}, RawAPI) ->
    #{endpoints := Endpoints} = API = namespace_refs(BasePath, RawAPI),
    API#{
        endpoints => [
            Endpoint#{path => <<BasePath/binary, Path/binary>>, base_path => BasePath}
         || #{path := Path} = Endpoint <- Endpoints
        ]
    }.

-spec namespace_refs(BasePath, API) -> NamespacedAPI when
    BasePath :: base_path(),
    API :: api(),
    NamespacedAPI :: api().
namespace_refs(<<>>, API) ->
    API;
namespace_refs(<<"/", Path/binary>>, API) ->
    Prefix = erf_util:to_snake_case(Path),
    #{schemas := Schemas} = RenamedAPI = rename_refs(<<Prefix/binary, "_">>, API),
    RenamedAPI#{
        schemas => maps:from_list([
            {<<Prefix/binary, "_", Ref/binary>>, Schema}
         || {Ref, Schema} <- maps:to_list(Schemas)
        ])
    }.

-spec rename_refs(Prefix, Term) -> RenamedTerm when
    Prefix :: binary(),
    Term :: term(),
    RenamedTerm :: term().
rename_refs(Prefix, Term) when is_map(Term) ->
    maps:map(
        fun
            (ref, Ref) when is_binary(Ref) ->
                <<Prefix/binary, Ref/binary>>;
            (_Key, Value) ->
                rename_refs(Prefix, Value)
        end,
        Term
    );
rename_refs(Prefix, Term) when is_list(Term) ->
    [rename_refs(Prefix, Item) || Item <- Term];
rename_refs(_Prefix, Term) ->
    Term.

-spec conflicting_routes(APIs) -> Result when
    APIs :: [api()],
    Result :: {ok, Path, OtherPath} | none,
    Path :: binary(),
    OtherPath :: binary().
conflicting_routes([]) ->
    none;
conflicting_routes([API | OtherAPIs]) ->
    Conflicts = [
        {Path, OtherPath}
     || #{path := Path} <- maps:get(endpoints, API),
        OtherAPI <- OtherAPIs,
        #{path := OtherPath} <- maps:get(endpoints, OtherAPI),
        segments_match(path_segments(Path), path_segments(OtherPath))
    ],
    case Conflicts of
        [{Path, OtherPath} | _Rest] ->
            {ok, Path, OtherPath};
        [] ->
            conflicting_routes(OtherAPIs)
    end.

-spec segments_match(Segments, OtherSegments) -> Match when
    Segments :: [binary()],
    OtherSegments :: [binary()],
    Match :: boolean().
segments_match([], []) ->
    true;
segments_match([Segment | Segments], [Segment | OtherSegments]) ->
    segments_match(Segments, OtherSegments);
segments_match([<<"{", _/binary>> | Segments], [_OtherSegment | OtherSegments]) ->
    segments_match(Segments, OtherSegments);
segments_match([_Segment | Segments], [<<"{", _/binary>> | OtherSegments]) ->
    segments_match(Segments, OtherSegments);
segments_match(_Segments, _OtherSegments) ->
    false.

-spec path_segments(Path) -> Segments when
    Path :: binary(),
    Segments :: [binary()].
path_segments(Path) ->
    [Segment || Segment <- binary:split(Path, <<"/">>, [global]), Segment =/= <<>>].

-spec swagger_routes(Mounts, SwaggerUI) -> Result when
    Mounts :: [mount()],
    SwaggerUI :: boolean(),
    Result :: {ok, [static_route()]} | {error, swagger_ui_not_found}.
swagger_routes(_Mounts, false) ->
    {ok, []};
swagger_routes(Mounts, true) ->
    case code:priv_dir(erf) of
        {error, bad_name} ->
            {error, swagger_ui_not_found};
        Priv ->
            IndexHTML = filename:join([Priv, <<"swagger-ui">>, <<"index.html">>]),
            {ok,
                lists:flatmap(
                    fun(#{base_path := BasePath, spec_path := SpecPath}) ->
                        [
                            {<<BasePath/binary, "/swagger">>, {file, IndexHTML}},
                            {<<BasePath/binary, "/swagger/spec.json">>, {file, SpecPath}}
                        ]
                    end,
                    Mounts
                )}
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

-spec match_route_(RawPath, RoutePatterns) -> Result when
    RawPath :: binary(),
    RoutePatterns :: erf:route_patterns(),
    Result :: {ok, Route} | {error, not_found},
    Route :: binary().
match_route_(_RawPath, []) ->
    {error, not_found};
match_route_(RawPath, [{Route, RouteRegEx} | Routes]) ->
    case re:run(RawPath, RouteRegEx) of
        nomatch ->
            match_route_(RawPath, Routes);
        _Otherwise ->
            {ok, Route}
    end.

-spec route_patterns(API, StaticRoutes) -> RoutePatterns when
    API :: api(),
    StaticRoutes :: [static_route()],
    RoutePatterns :: route_patterns().
route_patterns(API, StaticRoutes) ->
    Acc =
        lists:map(
            fun
                ({Path, {file, _ResourcePath}}) ->
                    {Path, <<"^", Path/binary, "$">>};
                ({Path, {dir, _ResourcePath}}) ->
                    {Path, <<"^", Path/binary>>}
            end,
            StaticRoutes
        ),
    RawRoutes = [maps:get(path, Endpoint) || Endpoint <- maps:get(endpoints, API)],
    route_patterns_(RawRoutes, Acc).

-spec route_patterns_(RawRoutes, Acc) -> RoutePatterns when
    RawRoutes :: [binary()],
    Acc :: route_patterns(),
    RoutePatterns :: route_patterns().
route_patterns_([], Acc) ->
    Acc;
route_patterns_([Route | Routes], Acc) ->
    RegExParts = lists:map(
        fun
            (<<"{", _Variable/binary>>) ->
                ?URL_ENCODED_STRING_REGEX;
            (Part) ->
                Part
        end,
        erlang:tl(string:split(Route, <<"/">>, all))
    ),
    RegEx =
        <<"^",
            (erlang:list_to_binary([
                <<"/">> | lists:join(<<"/">>, RegExParts)
            ]))/binary, "$">>,
    route_patterns_(Routes, [{Route, RegEx} | Acc]).
