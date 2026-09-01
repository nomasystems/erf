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
    mounts => [mount(), ...],
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

    RawConf = maps:merge(OldConf, NewConf),

    case reload_mounts(NewConf, RawConf) of
        {ok, Mounts} ->
            Conf = with_mounts(RawConf, Mounts),
            case build_router(Conf) of
                {ok, Extras} ->
                    erf_conf:set(Name, maps:merge(Conf, Extras)),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([Name, RawConf]) ->
    case mounts(RawConf) of
        {ok, Mounts} ->
            RawErfConf = with_mounts(
                #{
                    spec_parser => maps:get(spec_parser, RawConf, erf_parser_oas_3_0),
                    static_routes => maps:get(static_routes, RawConf, []),
                    swagger_ui => maps:get(swagger_ui, RawConf, false),
                    preprocess_middlewares => maps:get(preprocess_middlewares, RawConf, []),
                    postprocess_middlewares => maps:get(postprocess_middlewares, RawConf, []),
                    log_level => maps:get(log_level, RawConf, error)
                },
                Mounts
            ),

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
            end;
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
    Mounts = maps:get(mounts, Conf),
    RawStaticRoutes = maps:get(static_routes, Conf),
    SwaggerUI = maps:get(swagger_ui, Conf),

    case parse_api(Mounts) of
        {ok, API} ->
            Schemas = maps:to_list(maps:get(schemas, API)),
            case build_dtos(Schemas) of
                ok ->
                    StaticRoutes = swagger_routes(Mounts, SwaggerUI) ++ RawStaticRoutes,
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

-spec callbacks(Mounts) -> Callbacks when
    Mounts :: [mount(), ...],
    Callbacks :: #{base_path() => module()}.
callbacks(Mounts) ->
    maps:from_list([
        {maps:get(base_path, Mount), maps:get(callback, Mount)}
     || Mount <- Mounts
    ]).

-spec mounts(Conf) -> Result when
    Conf :: conf() | erf_conf:t(),
    Result :: {ok, Mounts} | {error, Reason},
    Mounts :: [mount(), ...],
    Reason :: term().
%% @doc Normalises the specification config into a list of mounts, so that a single
%% specification is just a single mount at the root.
mounts(#{mounts := _Mounts, spec_path := _SpecPath}) ->
    {error, {invalid_conf, mounts_and_spec_path}};
mounts(#{mounts := _Mounts, callback := _Callback}) ->
    {error, {invalid_conf, mounts_and_callback}};
mounts(#{mounts := []}) ->
    {error, {invalid_conf, empty_mounts}};
mounts(#{mounts := Mounts} = Conf) when is_list(Mounts) ->
    normalize_mounts(Mounts, default_spec_parser(Conf), [], []);
mounts(#{spec_path := SpecPath, callback := Callback} = Conf) ->
    normalize_mounts(
        [#{base_path => <<"/">>, spec_path => SpecPath, callback => Callback}],
        default_spec_parser(Conf),
        [],
        []
    );
mounts(_Conf) ->
    {error, {invalid_conf, missing_spec_path}}.

-spec with_mounts(Conf, Mounts) -> NewConf when
    Conf :: erf_conf:t(),
    Mounts :: [mount(), ...],
    NewConf :: erf_conf:t().
%% @doc Stores the normalised mounts in a configuration. An instance serving a single
%% specification from the root keeps `spec_path' and `callback' alongside them, so that a
%% configuration that never mentions `mounts' reads back exactly as it always has.
with_mounts(Conf, [#{base_path := <<>>, spec_path := SpecPath, callback := Callback}] = Mounts) ->
    Conf#{mounts => Mounts, spec_path => SpecPath, callback => Callback};
with_mounts(Conf, Mounts) ->
    (maps:without([spec_path, callback], Conf))#{mounts => Mounts}.

-spec reload_mounts(NewConf, RawConf) -> Result when
    NewConf :: erf_conf:t(),
    RawConf :: erf_conf:t(),
    Result :: {ok, Mounts} | {error, Reason},
    Mounts :: [mount(), ...],
    Reason :: term().
%% @doc Resolves the mounts of a reloaded configuration. A reload carrying `mounts' replaces
%% the stored ones wholesale, while one carrying `spec_path' or `callback' patches the only
%% mount already configured, which is how an instance serving one specification swaps its
%% callback module.
%% The stored configuration of a single-specification instance carries `spec_path' and
%% `callback' next to its `mounts', so the exclusivity between the two shapes is checked
%% against what the reload itself brings, never against the merged configuration.
reload_mounts(NewConf, RawConf) ->
    HasMounts = maps:is_key(mounts, NewConf),
    HasSingleSpec = maps:is_key(spec_path, NewConf) orelse maps:is_key(callback, NewConf),
    case {HasMounts, HasSingleSpec} of
        {true, true} ->
            mounts(NewConf);
        {true, false} ->
            mounts(maps:without([spec_path, callback], RawConf));
        {false, true} ->
            patch_mount(NewConf, RawConf);
        {false, false} ->
            stored_mounts(RawConf)
    end.

-spec stored_mounts(RawConf) -> Result when
    RawConf :: erf_conf:t(),
    Result :: {ok, Mounts} | {error, Reason},
    Mounts :: [mount(), ...],
    Reason :: term().
%% @doc Returns the mounts a reload that brings no specification config of its own must keep.
%% They are already normalised, unless nothing has been configured for this instance yet.
stored_mounts(#{mounts := Mounts}) ->
    {ok, Mounts};
stored_mounts(RawConf) ->
    mounts(RawConf).

-spec patch_mount(NewConf, RawConf) -> Result when
    NewConf :: erf_conf:t(),
    RawConf :: erf_conf:t(),
    Result :: {ok, Mounts} | {error, Reason},
    Mounts :: [mount(), ...],
    Reason :: term().
patch_mount(#{spec_path := SpecPath, callback := Callback}, RawConf) ->
    normalize_mounts(
        [#{base_path => <<"/">>, spec_path => SpecPath, callback => Callback}],
        default_spec_parser(RawConf),
        [],
        []
    );
patch_mount(NewConf, RawConf) ->
    Patch = maps:with([spec_path, callback, spec_parser], NewConf),
    case maps:get(mounts, RawConf, undefined) of
        undefined ->
            mounts(RawConf);
        [Mount] ->
            normalize_mounts(
                [maps:merge(Mount, Patch)], default_spec_parser(RawConf), [], []
            );
        _Mounts ->
            {error, {invalid_conf, ambiguous_mount_patch}}
    end.

-spec default_spec_parser(Conf) -> SpecParser when
    Conf :: conf() | erf_conf:t(),
    SpecParser :: module().
default_spec_parser(Conf) ->
    maps:get(spec_parser, Conf, erf_parser_oas_3_0).

-spec normalize_mounts(Mounts, DefaultSpecParser, SeenBasePaths, Acc) -> Result when
    Mounts :: [mount()],
    DefaultSpecParser :: module(),
    SeenBasePaths :: [base_path()],
    Acc :: [mount()],
    Result :: {ok, [mount()]} | {error, Reason},
    Reason :: term().
normalize_mounts([], _DefaultSpecParser, _SeenBasePaths, Acc) ->
    {ok, lists:reverse(Acc)};
normalize_mounts([Mount | Rest], DefaultSpecParser, SeenBasePaths, Acc) ->
    case normalize_mount(Mount, DefaultSpecParser) of
        {ok, #{base_path := BasePath} = NormalizedMount} ->
            case lists:member(BasePath, SeenBasePaths) of
                true ->
                    {error, {duplicate_base_path, BasePath}};
                false ->
                    normalize_mounts(
                        Rest,
                        DefaultSpecParser,
                        [BasePath | SeenBasePaths],
                        [NormalizedMount | Acc]
                    )
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec normalize_mount(Mount, DefaultSpecParser) -> Result when
    Mount :: mount(),
    DefaultSpecParser :: module(),
    Result :: {ok, mount()} | {error, Reason},
    Reason :: term().
normalize_mount(
    #{base_path := RawBasePath, spec_path := SpecPath, callback := Callback} = Mount,
    DefaultSpecParser
) when is_binary(RawBasePath), is_binary(SpecPath), is_atom(Callback) ->
    case normalize_base_path(RawBasePath) of
        {ok, BasePath} ->
            {ok, #{
                base_path => BasePath,
                spec_path => SpecPath,
                callback => Callback,
                spec_parser => maps:get(spec_parser, Mount, DefaultSpecParser)
            }};
        {error, Reason} ->
            {error, Reason}
    end;
normalize_mount(Mount, _DefaultSpecParser) ->
    {error, {invalid_mount, Mount}}.

-spec normalize_base_path(RawBasePath) -> Result when
    RawBasePath :: binary(),
    Result :: {ok, base_path()} | {error, Reason},
    Reason :: term().
%% @doc Canonicalises a base path so that the root is the empty binary and every other base
%% path has a leading and no trailing slash, making it a prefix that can be prepended as is.
normalize_base_path(RawBasePath) ->
    Segments = path_segments(RawBasePath),
    case lists:any(fun is_path_parameter/1, Segments) of
        true ->
            {error, {invalid_base_path, RawBasePath}};
        false ->
            {ok, erlang:iolist_to_binary([[<<"/">>, Segment] || Segment <- Segments])}
    end.

-spec parse_api(Mounts) -> Result when
    Mounts :: [mount(), ...],
    Result :: {ok, API} | {error, Reason},
    API :: api(),
    Reason :: term().
%% @doc Parses the specification of every mount and merges them into a single API AST whose
%% endpoints are prefixed by, and tagged with, the base path they are mounted under.
parse_api(Mounts) ->
    case parse_mounts(Mounts, []) of
        {ok, ParsedMounts} ->
            merge_apis(ParsedMounts);
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_mounts(Mounts, Acc) -> Result when
    Mounts :: [mount()],
    Acc :: [{mount(), api()}],
    Result :: {ok, [{mount(), api()}]} | {error, Reason},
    Reason :: term().
parse_mounts([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_mounts([Mount | Rest], Acc) ->
    SpecPath = maps:get(spec_path, Mount),
    SpecParser = maps:get(spec_parser, Mount),
    case erf_parser:parse(SpecPath, SpecParser) of
        {ok, API} ->
            parse_mounts(Rest, [{Mount, API} | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec merge_apis(ParsedMounts) -> Result when
    ParsedMounts :: [{mount(), api()}, ...],
    Result :: {ok, API} | {error, Reason},
    API :: api(),
    Reason :: term().
%% @doc Merges the APIs parsed from every mount into a single API AST. The parser already
%% namespaces the schemas it generates by the specification's file name, so references are
%% only renamed for the mounts whose specifications share a file name with another mount's.
merge_apis([{_FirstMount, FirstAPI} | _Rest] = ParsedMounts) ->
    Ambiguous = ambiguous_spec_names(ParsedMounts),
    PrefixedAPIs = [
        prefix_api(Mount, API, lists:member(spec_name(Mount), Ambiguous))
     || {Mount, API} <- ParsedMounts
    ],
    Endpoints = lists:flatmap(fun(API) -> maps:get(endpoints, API) end, PrefixedAPIs),
    case conflicting_routes(PrefixedAPIs) of
        {ok, Path, OtherPath} ->
            {error, {conflicting_routes, Path, OtherPath}};
        none ->
            Schemas = lists:foldl(
                fun(API, Acc) -> maps:merge(Acc, maps:get(schemas, API)) end,
                #{},
                PrefixedAPIs
            ),
            {ok, FirstAPI#{endpoints => Endpoints, schemas => Schemas}}
    end.

-spec prefix_api(Mount, API, PrefixRefs) -> NewAPI when
    Mount :: mount(),
    API :: api(),
    PrefixRefs :: boolean(),
    NewAPI :: api().
prefix_api(#{base_path := BasePath}, API, PrefixRefs) ->
    Prefix =
        case PrefixRefs of
            true -> ref_prefix(BasePath);
            false -> undefined
        end,
    Endpoints = [
        (rewrite_refs(Prefix, Endpoint))#{
            path => <<BasePath/binary, (maps:get(path, Endpoint))/binary>>,
            base_path => BasePath
        }
     || Endpoint <- maps:get(endpoints, API)
    ],
    Schemas = maps:fold(
        fun(Ref, Schema, Acc) ->
            Acc#{prefix_ref(Prefix, Ref) => rewrite_refs(Prefix, Schema)}
        end,
        #{},
        maps:get(schemas, API)
    ),
    API#{endpoints => Endpoints, schemas => Schemas}.

-spec ambiguous_spec_names(ParsedMounts) -> SpecNames when
    ParsedMounts :: [{mount(), api()}],
    SpecNames :: [binary()].
%% @doc Returns the specification file names used by more than one mount, whose generated
%% schema references would otherwise collide.
ambiguous_spec_names(ParsedMounts) ->
    Counts = lists:foldl(
        fun({Mount, _API}, Acc) ->
            SpecName = spec_name(Mount),
            Acc#{SpecName => maps:get(SpecName, Acc, 0) + 1}
        end,
        #{},
        ParsedMounts
    ),
    [SpecName || SpecName := Count <- Counts, Count > 1].

-spec spec_name(Mount) -> SpecName when
    Mount :: mount(),
    SpecName :: binary().
spec_name(#{spec_path := SpecPath}) ->
    filename:rootname(filename:basename(SpecPath)).

-spec conflicting_routes(PrefixedAPIs) -> Result when
    PrefixedAPIs :: [api()],
    Result :: {ok, Path, OtherPath} | none,
    Path :: binary(),
    OtherPath :: binary().
%% @doc Finds two routes belonging to different mounts that can match the same
%% request, which the generated router would silently resolve by clause order. Routes within
%% a single specification are left alone: a specification is free to shadow, say,
%% `/items/{id}' with `/items/latest', and the more specific one is expected to win.
conflicting_routes([]) ->
    none;
conflicting_routes([API | Rest]) ->
    Paths = [maps:get(path, Endpoint) || Endpoint <- maps:get(endpoints, API)],
    OtherPaths = [
        maps:get(path, Endpoint)
     || OtherAPI <- Rest, Endpoint <- maps:get(endpoints, OtherAPI)
    ],
    Conflicts = [
        {Path, OtherPath}
     || Path <- Paths, OtherPath <- OtherPaths, paths_conflict(Path, OtherPath)
    ],
    case Conflicts of
        [{Path, OtherPath} | _Rest] ->
            {ok, Path, OtherPath};
        [] ->
            conflicting_routes(Rest)
    end.

-spec paths_conflict(Path, OtherPath) -> Conflict when
    Path :: binary(),
    OtherPath :: binary(),
    Conflict :: boolean().
paths_conflict(Path, OtherPath) ->
    Segments = path_segments(Path),
    OtherSegments = path_segments(OtherPath),
    erlang:length(Segments) =:= erlang:length(OtherSegments) andalso
        lists:all(
            fun({Segment, OtherSegment}) ->
                Segment =:= OtherSegment orelse
                    is_path_parameter(Segment) orelse
                    is_path_parameter(OtherSegment)
            end,
            lists:zip(Segments, OtherSegments)
        ).

-spec path_segments(Path) -> Segments when
    Path :: binary(),
    Segments :: [binary()].
path_segments(Path) ->
    [Segment || Segment <- binary:split(Path, <<"/">>, [global]), Segment =/= <<>>].

-spec is_path_parameter(Segment) -> IsPathParameter when
    Segment :: binary(),
    IsPathParameter :: boolean().
is_path_parameter(<<"{", _Rest/binary>>) ->
    true;
is_path_parameter(_Segment) ->
    false.

-spec rewrite_refs(Prefix, Term) -> NewTerm when
    Prefix :: binary() | undefined,
    Term :: term(),
    NewTerm :: term().
%% @doc Recursively walks an API AST fragment, namespacing every `ref' field it finds
%% (schema/parameter/body references) no matter how deeply nested it is, inside object
%% properties, array items, oneOf/anyOf branches and so on.
rewrite_refs(undefined, Term) ->
    Term;
rewrite_refs(Prefix, Term) when is_map(Term) ->
    maps:fold(
        fun
            (ref, Ref, Acc) when is_binary(Ref) ->
                Acc#{ref => prefix_ref(Prefix, Ref)};
            (Key, Value, Acc) ->
                Acc#{Key => rewrite_refs(Prefix, Value)}
        end,
        #{},
        Term
    );
rewrite_refs(Prefix, Term) when is_list(Term) ->
    [rewrite_refs(Prefix, Item) || Item <- Term];
rewrite_refs(_Prefix, Term) ->
    Term.

-spec prefix_ref(Prefix, Ref) -> NewRef when
    Prefix :: binary() | undefined,
    Ref :: erf_parser:ref(),
    NewRef :: erf_parser:ref().
prefix_ref(undefined, Ref) ->
    Ref;
prefix_ref(Prefix, Ref) ->
    <<Prefix/binary, "_", Ref/binary>>.

-spec ref_prefix(BasePath) -> Prefix when
    BasePath :: base_path(),
    Prefix :: binary().
%% @doc Turns a base path into a valid prefix for a generated schema reference.
ref_prefix(<<>>) ->
    <<"root">>;
ref_prefix(BasePath) ->
    erf_util:to_snake_case(
        binary:replace(
            string:trim(BasePath, leading, "/"), [<<"/">>, <<"-">>], <<"_">>, [global]
        )
    ).

-spec swagger_routes(Mounts, SwaggerUI) -> StaticRoutes when
    Mounts :: [mount(), ...],
    SwaggerUI :: boolean(),
    StaticRoutes :: [static_route()].
%% @doc Serves a Swagger UI per mount, under the base path the mount is served from, so that
%% each mount documents only its own endpoints.
swagger_routes(_Mounts, false) ->
    [];
swagger_routes(Mounts, true) ->
    IndexHTML =
        case code:priv_dir(erf) of
            {error, bad_name} ->
                {error, <<"Cannot build `swagger-ui`">>};
            Priv ->
                filename:join([Priv, <<"swagger-ui">>, <<"index.html">>])
        end,
    lists:flatmap(
        fun(#{base_path := BasePath, spec_path := SpecPath}) ->
            [
                {<<BasePath/binary, "/swagger">>, {file, IndexHTML}},
                {<<BasePath/binary, "/swagger/spec.json">>, {file, SpecPath}}
            ]
        end,
        Mounts
    ).

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
