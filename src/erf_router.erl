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

%% <code>erf</code>'s router module.
-module(erf_router).

%%% INCLUDE FILES
-include("erf_generator.hrl").
-include_lib("kernel/include/logger.hrl").

%%% EXTERNAL EXPORTS
-export([
    generate/2,
    load/1,
    handle/2
]).

%%% TYPES
-type t() :: erl_syntax:syntaxTree().
-type callback() :: module().
-type callback_spec() :: callback() | #{erf:version() => callback()}.
-type generator_opts() :: #{callback := callback_spec(), static_routes := [erf:static_route()]}.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(API, Opts) -> Result when
    API :: erf:api(),
    Opts :: generator_opts(),
    Result :: {Mod, Router},
    Mod :: module(),
    Router :: t().
%% @doc Generates an Erlang Syntax Tree of a router module from an API AST.
generate(API, Opts) ->
    ModuleName = erlang:binary_to_atom(
        erf_util:to_snake_case(
            <<(maps:get(name, API))/binary, "_router">>
        )
    ),
    ModuleHeader = erl_syntax:comment(?COPYRIGHT ++ [?NOTE]),
    ModuleAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModuleName)]),
    ExportHeader = erl_syntax:comment([?EXPORTS_HEADER]),
    ExportAttr = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(handle), erl_syntax:integer(1))
        ])
    ]),
    ExportHeader2 = erl_syntax:comment([?CLINE, ?EXPORTS_HEADER, ?CLINE]),

    HandleFun = handle_ast(API, Opts),

    Router = erl_syntax:form_list(
        lists:append([
            [
                erl_syntax:set_precomments(
                    ModuleAttr,
                    [ModuleHeader]
                ),
                erl_syntax:set_precomments(
                    ExportAttr,
                    [ExportHeader]
                ),
                erl_syntax:set_precomments(
                    HandleFun,
                    [ExportHeader2]
                )
            ]
        ])
    ),
    {ModuleName, Router}.

-spec load(Router) -> Result when
    Router :: t(),
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
%% @doc Loads a router module into the Erlang Runtime System.
load(Router) ->
    Forms = erl_syntax:revert_forms(Router),
    case compile:forms(Forms, []) of
        {ok, ModuleName, Bin} when is_atom(ModuleName) andalso is_binary(Bin) ->
            case load_binary(ModuleName, Bin) of
                ok ->
                    ok;
                {error, What} ->
                    {error, {[What], []}}
            end;
        {ok, ModuleName, Bin, Warnings} when is_atom(ModuleName) andalso is_binary(Bin) ->
            case load_binary(ModuleName, Bin) of
                ok ->
                    {ok, Warnings};
                {error, What} ->
                    {error, {[What], Warnings}}
            end;
        {error, Errors, Warnings} ->
            {error, {Errors, Warnings}};
        error ->
            error
    end.

-spec handle(Name, Request) -> Result when
    Name :: atom(),
    Request :: erf:request(),
    Result :: erf:response().
%% @doc Handles an HTTP request.
handle(Name, RawRequest) ->
    {ok, PreProcessMiddlewares} = erf_conf:preprocess_middlewares(Name),
    {ok, RouterMod} = erf_conf:router_mod(Name),
    {ok, PostProcessMiddlewares} = erf_conf:postprocess_middlewares(Name),
    {InitialResponse, InitialRequest} =
        case preprocess(RawRequest) of
            {ok, Request} ->
                case apply_preprocess_middlewares(Request, PreProcessMiddlewares) of
                    {stop, PreprocessResponse, PreprocessRequest} ->
                        {PreprocessResponse, PreprocessRequest};
                    PreprocessRequest ->
                        {RouterMod:handle(PreprocessRequest), PreprocessRequest}
                end;
            {error, _Reason} ->
                ContentTypeHeader = string:casefold(<<"content-type">>),
                %% `postprocess/2' owns the encode. An encoded body here becomes a JSON string.
                ErrorBody = #{
                    <<"title">> => <<"Bad Request">>,
                    <<"status">> => 400,
                    <<"detail">> => <<"Failed to read request">>
                },
                ResponseError = {400, [{ContentTypeHeader, <<"application/json">>}], ErrorBody},
                {ResponseError, RawRequest}
        end,
    Response = apply_postprocess_middlewares(
        InitialRequest, InitialResponse, PostProcessMiddlewares
    ),
    postprocess(InitialRequest, Response).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec apply_preprocess_middlewares(Request, Middlewares) -> Result when
    Request :: erf:request(),
    Middlewares :: [erf_preprocess_middleware:t()],
    Result :: erf:request() | {stop, erf:response(), erf:request()} | {stop, erf:response()}.
apply_preprocess_middlewares(Request, []) ->
    Request;
apply_preprocess_middlewares(RawRequest, [Middleware | Rest]) ->
    case Middleware:preprocess(RawRequest) of
        {stop, Response} ->
            {stop, Response, RawRequest};
        {stop, Response, Request} ->
            {stop, Response, Request};
        Request ->
            apply_preprocess_middlewares(Request, Rest)
    end.

-spec apply_postprocess_middlewares(Request, Response, Middlewares) -> Result when
    Request :: erf:request(),
    Response :: erf:response(),
    Middlewares :: [erf_postprocess_middleware:t()],
    Result :: erf:response().
apply_postprocess_middlewares(_Request, Response, []) ->
    Response;
apply_postprocess_middlewares(Request, RawResponse, [Middleware | Rest]) ->
    case Middleware:postprocess(Request, RawResponse) of
        {Response, NewRequest} ->
            apply_postprocess_middlewares(NewRequest, Response, Rest);
        Response ->
            apply_postprocess_middlewares(Request, Response, Rest)
    end.

-spec handle_ast(API, Opts) -> Result when
    API :: erf:api(),
    Opts :: generator_opts(),
    Result :: t().
handle_ast(API, #{callback := Callback} = Opts) ->
    RESTClauses = lists:flatmap(
        fun(Endpoint) ->
            RawPath = lists:filter(
                fun
                    (<<>>) ->
                        false;
                    (_Part) ->
                        true
                end,
                binary:split(maps:get(path, Endpoint), [<<"/">>], [global])
            ),
            Path = erl_syntax:list(
                lists:map(
                    fun
                        (<<${, Rest/binary>>) ->
                            ParameterName = erf_util:to_pascal_case(
                                string:trim(Rest, trailing, [$}])
                            ),
                            erl_syntax:variable(erlang:binary_to_atom(ParameterName));
                        (Segment) ->
                            erl_syntax:binary([
                                erl_syntax:binary_field(
                                    erl_syntax:string(erlang:binary_to_list(Segment))
                                )
                            ])
                    end,
                    RawPath
                )
            ),
            EndpointParameters = maps:get(parameters, Endpoint),
            Version = maps:get(version, Endpoint, undefined),
            EndpointCallback = resolve_callback(Callback, Version),
            Operations = maps:get(operations, Endpoint, []),
            AllowedMethods = lists:map(
                fun(Operation) ->
                    Method = erl_syntax:atom(
                        maps:get(method, Operation)
                    ),
                    OperationParameters = maps:get(parameters, Operation),
                    Parameters = EndpointParameters ++ OperationParameters,
                    PathParameters = lists:filter(
                        fun(Parameter) ->
                            maps:get(type, Parameter) =:= path
                        end,
                        Parameters
                    ),
                    Request = maps:get(request, Operation),

                    PathParametersAST = erl_syntax:list(
                        lists:map(
                            fun(Parameter) ->
                                ParameterName = maps:get(name, Parameter),
                                erl_syntax:tuple([
                                    erl_syntax:binary([
                                        erl_syntax:binary_field(
                                            erl_syntax:string(erlang:binary_to_list(ParameterName))
                                        )
                                    ]),
                                    erl_syntax:variable(
                                        erlang:binary_to_atom(
                                            erf_util:to_pascal_case(ParameterName)
                                        )
                                    )
                                ])
                            end,
                            PathParameters
                        )
                    ),
                    #{
                        bindings := ParameterBindings,
                        is_valid_request := IsValidRequestAST,
                        sources := SourcesAST,
                        presence := PresenceAST
                    } = request_validation(Parameters, Request),

                    erl_syntax:clause(
                        [
                            erl_syntax:match_expr(
                                erl_syntax:variable('Request'),
                                erl_syntax:map_expr(
                                    none,
                                    [
                                        erl_syntax:map_field_exact(
                                            erl_syntax:atom(path),
                                            Path
                                        ),
                                        erl_syntax:map_field_exact(
                                            erl_syntax:atom(method),
                                            Method
                                        ),
                                        erl_syntax:map_field_exact(
                                            erl_syntax:atom(query_parameters),
                                            erl_syntax:variable('QueryParameters')
                                        ),
                                        erl_syntax:map_field_exact(
                                            erl_syntax:atom(body),
                                            erl_syntax:variable('Body')
                                        ),
                                        erl_syntax:map_field_exact(
                                            erl_syntax:atom(headers),
                                            erl_syntax:variable('Headers')
                                        )
                                    ]
                                )
                            )
                        ],
                        none,
                        [
                            erl_syntax:match_expr(
                                erl_syntax:variable('PathParameters'),
                                PathParametersAST
                            )
                        ] ++ ParameterBindings ++
                            [
                                erl_syntax:match_expr(
                                    erl_syntax:variable('IsValidRequest'),
                                    IsValidRequestAST
                                ),
                                erl_syntax:case_expr(
                                    erl_syntax:variable('IsValidRequest'),
                                    [
                                        erl_syntax:clause(
                                            [erl_syntax:atom(true)],
                                            none,
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:atom(EndpointCallback),
                                                    erl_syntax:atom(
                                                        erlang:binary_to_atom(
                                                            erf_util:to_snake_case(
                                                                maps:get(id, Operation)
                                                            ),
                                                            utf8
                                                        )
                                                    ),
                                                    [
                                                        erl_syntax:map_expr(
                                                            erl_syntax:variable('Request'),
                                                            request_extra_fields(Version)
                                                        )
                                                    ]
                                                )
                                            ]
                                        ),
                                        erl_syntax:clause(
                                            [
                                                erl_syntax:tuple([
                                                    erl_syntax:atom(false),
                                                    erl_syntax:variable('Erf_Reason')
                                                ])
                                            ],
                                            none,
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:atom(erf_validation),
                                                    erl_syntax:atom(bad_request),
                                                    [
                                                        erl_syntax:variable('Erf_Reason'),
                                                        SourcesAST,
                                                        PresenceAST
                                                    ]
                                                )
                                            ]
                                        )
                                    ]
                                )
                            ]
                    )
                end,
                Operations
            ),
            NotAllowedMethod = method_not_allowed_clause(Path, Operations),

            AllowedMethods ++ [NotAllowedMethod]
        end,
        maps:get(endpoints, API, [])
    ),
    StaticRoutes = maps:get(static_routes, Opts, []),
    StaticClauses =
        lists:map(
            fun({Path, Resource}) ->
                PathSegments = lists:filter(
                    fun
                        (<<>>) ->
                            false;
                        (_Part) ->
                            true
                    end,
                    binary:split(Path, [<<"/">>], [global])
                ),
                {PatternPathAST, FilePathAST} =
                    case Resource of
                        {file, File} ->
                            PatternPath =
                                erl_syntax:list(
                                    lists:map(
                                        fun(Segment) ->
                                            erl_syntax:binary([
                                                erl_syntax:binary_field(
                                                    erl_syntax:string(
                                                        erlang:binary_to_list(Segment)
                                                    )
                                                )
                                            ])
                                        end,
                                        PathSegments
                                    )
                                ),
                            FilePath =
                                erl_syntax:binary([
                                    erl_syntax:binary_field(
                                        erl_syntax:string(erlang:binary_to_list(File))
                                    )
                                ]),
                            {PatternPath, FilePath};
                        {dir, Dir} ->
                            PatternPath =
                                erl_syntax:list(
                                    lists:map(
                                        fun(Segment) ->
                                            erl_syntax:binary([
                                                erl_syntax:binary_field(
                                                    erl_syntax:string(
                                                        erlang:binary_to_list(Segment)
                                                    )
                                                )
                                            ])
                                        end,
                                        PathSegments
                                    ),
                                    erl_syntax:variable('Resource')
                                ),
                            GetFile =
                                erl_syntax:application(
                                    erl_syntax:atom(filename),
                                    erl_syntax:atom(join),
                                    [
                                        erl_syntax:list(
                                            [
                                                erl_syntax:binary([
                                                    erl_syntax:binary_field(
                                                        erl_syntax:string(
                                                            erlang:binary_to_list(Dir)
                                                        )
                                                    )
                                                ])
                                            ],
                                            erl_syntax:variable('Resource')
                                        )
                                    ]
                                ),
                            {PatternPath, GetFile}
                    end,
                erl_syntax:clause(
                    [
                        erl_syntax:map_expr(
                            none,
                            [
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(path),
                                    PatternPathAST
                                ),
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(method),
                                    erl_syntax:atom(get)
                                ),
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(query_parameters),
                                    erl_syntax:variable('_QueryParameters')
                                ),
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(headers),
                                    erl_syntax:variable('_Headers')
                                ),
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(body),
                                    erl_syntax:variable('_Body')
                                ),
                                erl_syntax:map_field_exact(
                                    erl_syntax:atom(peer),
                                    erl_syntax:variable('_Peer')
                                )
                            ]
                        )
                    ],
                    none,
                    [
                        erl_syntax:match_expr(
                            erl_syntax:variable('File'),
                            FilePathAST
                        ),
                        erl_syntax:tuple([
                            erl_syntax:integer(200),
                            erl_syntax:list([
                                erl_syntax:tuple([
                                    erl_syntax:binary([
                                        erl_syntax:binary_field(
                                            erl_syntax:string("content-type")
                                        )
                                    ]),
                                    erl_syntax:application(
                                        erl_syntax:atom(erf_static),
                                        erl_syntax:atom(mime_type),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom(filename),
                                                erl_syntax:atom(extension),
                                                [erl_syntax:variable('File')]
                                            )
                                        ]
                                    )
                                ])
                            ]),
                            erl_syntax:tuple([erl_syntax:atom(file), erl_syntax:variable('File')])
                        ])
                    ]
                )
            end,
            StaticRoutes
        ),
    NotFoundClause =
        erl_syntax:clause(
            [
                erl_syntax:variable('_Req')
            ],
            none,
            [
                erl_syntax:tuple(
                    [
                        erl_syntax:integer(404),
                        erl_syntax:list([]),
                        erl_syntax:atom(undefined)
                    ]
                )
            ]
        ),
    erl_syntax:function(
        erl_syntax:atom(handle),
        RESTClauses ++ StaticClauses ++ [NotFoundClause]
    ).

-spec method_not_allowed_clause(Path, Operations) -> Clause when
    Path :: erl_syntax:syntaxTree(),
    Operations :: [erf_parser:operation()],
    Clause :: erl_syntax:syntaxTree().
-doc """
Builds the clause that answers a method the endpoint does not define.

The spec fixes the set of supported methods, so the `Allow` header of RFC 9110
section 15.5.6 and the problem document are literals that the compiler lifts
into the module constant pool. A 405 costs no work at runtime.
""".
method_not_allowed_clause(Path, Operations) ->
    Allow = allow(Operations),
    erl_syntax:clause(
        [
            erl_syntax:match_expr(
                erl_syntax:variable('Request'),
                erl_syntax:map_expr(
                    none,
                    [
                        erl_syntax:map_field_exact(
                            erl_syntax:atom(path),
                            Path
                        ),
                        erl_syntax:map_field_exact(
                            erl_syntax:atom(method),
                            erl_syntax:variable('_Method')
                        )
                    ]
                )
            )
        ],
        none,
        [
            erl_syntax:tuple(
                [
                    erl_syntax:integer(405),
                    erl_syntax:list([
                        header(<<"allow">>, Allow),
                        header(<<"content-type">>, <<"application/problem+json">>)
                    ]),
                    binary_ast(method_not_allowed_body(Allow))
                ]
            )
        ]
    ).

-spec allow(Operations) -> Allow when
    Operations :: [erf_parser:operation()],
    Allow :: binary().
-doc """
Builds the value of the `Allow` header from the operations of an endpoint.

The router answers exactly the methods that the spec defines, so the header
names those and nothing else. `HEAD` and `OPTIONS` appear only when the spec
defines them, because the router has no implicit clause for either.
""".
allow(Operations) ->
    Methods = lists:usort([method_label(maps:get(method, Operation)) || Operation <- Operations]),
    erlang:iolist_to_binary(lists:join(<<", ">>, [Name || {_Rank, Name} <- Methods])).

-spec method_label(Method) -> Label when
    Method :: erf:method(),
    Label :: {Rank :: pos_integer(), Name :: binary()}.
%% The rank makes the header order stable across builds, whatever order the
%% parser hands the operations in. The name is the case-sensitive token of
%% RFC 9110 section 9.
method_label(get) -> {1, <<"GET">>};
method_label(head) -> {2, <<"HEAD">>};
method_label(post) -> {3, <<"POST">>};
method_label(put) -> {4, <<"PUT">>};
method_label(patch) -> {5, <<"PATCH">>};
method_label(delete) -> {6, <<"DELETE">>};
method_label(options) -> {7, <<"OPTIONS">>};
method_label(trace) -> {8, <<"TRACE">>};
method_label(connect) -> {9, <<"CONNECT">>}.

-spec method_not_allowed_body(Allow) -> Body when
    Allow :: binary(),
    Body :: binary().
-doc "Builds the RFC 9457 problem document of a 405 response.".
method_not_allowed_body(<<>>) ->
    method_not_allowed_problem(<<"The target resource does not support the request method.">>);
method_not_allowed_body(Allow) ->
    method_not_allowed_problem(
        <<"The target resource does not support the request method. Supported methods: ",
            Allow/binary, ".">>
    ).

-spec method_not_allowed_problem(Detail) -> Body when
    Detail :: binary(),
    Body :: binary().
method_not_allowed_problem(Detail) ->
    erlang:iolist_to_binary(
        json:encode(#{
            <<"type">> => <<"about:blank">>,
            <<"title">> => <<"Method Not Allowed">>,
            <<"status">> => 405,
            <<"detail">> => Detail
        })
    ).

-spec header(Name, Value) -> Header when
    Name :: binary(),
    Value :: binary(),
    Header :: erl_syntax:syntaxTree().
header(Name, Value) ->
    erl_syntax:tuple([binary_ast(Name), binary_ast(Value)]).

-spec binary_ast(Value) -> AST when
    Value :: binary(),
    AST :: erl_syntax:syntaxTree().
binary_ast(Value) ->
    erl_syntax:binary([
        erl_syntax:binary_field(erl_syntax:string(erlang:binary_to_list(Value)))
    ]).

-spec resolve_callback(CallbackSpec, Version) -> Callback when
    CallbackSpec :: callback_spec(),
    Version :: erf:version() | undefined,
    Callback :: callback().
%% @doc Resolves which callback module handles a given endpoint: a single callback module
%% (the only shape `erf` has ever supported) is shared by every endpoint, while a per-version
%% map gives each version's endpoints their own dedicated module.
resolve_callback(Callback, _Version) when is_atom(Callback) ->
    Callback;
resolve_callback(CallbacksByVersion, Version) when is_map(CallbacksByVersion) ->
    maps:get(Version, CallbacksByVersion).

-spec request_extra_fields(Version) -> Fields when
    Version :: erf:version() | undefined,
    Fields :: [erl_syntax:syntaxTree()].
%% @doc Builds the extra fields spliced into the `Request` map handed to the callback: the
%% resolved path parameters and, for endpoints belonging to a versioned API, the version they
%% were matched under (so a shared callback function can branch on it if it needs to).
request_extra_fields(undefined) ->
    [
        erl_syntax:map_field_assoc(
            erl_syntax:atom('path_parameters'),
            erl_syntax:variable('PathParameters')
        )
    ];
request_extra_fields(Version) ->
    [
        erl_syntax:map_field_assoc(
            erl_syntax:atom('path_parameters'),
            erl_syntax:variable('PathParameters')
        ),
        erl_syntax:map_field_assoc(
            erl_syntax:atom(version),
            erl_syntax:binary([
                erl_syntax:binary_field(
                    erl_syntax:string(erlang:binary_to_list(Version))
                )
            ])
        )
    ].

-spec request_validation(Parameters, Request) -> Result when
    Parameters :: [erf_parser:parameter()],
    Request :: erf_parser:request(),
    Result :: #{
        bindings := [erl_syntax:syntaxTree()],
        is_valid_request := erl_syntax:syntaxTree(),
        sources := erl_syntax:syntaxTree(),
        presence := erl_syntax:syntaxTree()
    }.
%% @doc Builds the validation section of an operation clause.
%%
%% `bindings' hoists every parameter value into its own variable so the failure
%% branch can tell an absent value from an invalid one without repeating the
%% lookup. `sources' and `presence' are the two tuples `erf_validation' needs to
%% turn an `ndto' reason into a problem document: the first is a literal that
%% names each condition, the second is evaluated only when validation fails.
request_validation(RawParameters, Request) ->
    RawRequestBody = maps:get(body, Request),
    RequestBodyRef = maps:get(ref, RawRequestBody),
    RequestBodyRequired = maps:get(required, RawRequestBody),
    RequestBodyModule =
        erlang:binary_to_atom(erf_util:to_snake_case(RequestBodyRef)),
    RequestBodyIsValid =
        erl_syntax:application(
            erl_syntax:atom(RequestBodyModule),
            erl_syntax:atom(is_valid),
            [erl_syntax:variable('Body')]
        ),
    RequestBody =
        case RequestBodyRequired of
            true ->
                RequestBodyIsValid;
            false ->
                erl_syntax:infix_expr(
                    erl_syntax:infix_expr(
                        erl_syntax:variable('Body'),
                        erl_syntax:operator('=:='),
                        erl_syntax:atom(undefined)
                    ),
                    erl_syntax:operator('orelse'),
                    RequestBodyIsValid
                )
        end,
    FilteredParameters =
        lists:filtermap(
            fun(Parameter) ->
                ParameterModule = erlang:binary_to_atom(maps:get(ref, Parameter)),
                ParameterName = maps:get(name, Parameter),
                ParameterType = maps:get(type, Parameter),
                ParameterSchema = maps:get(schema, Parameter, undefined),
                case ParameterType of
                    header ->
                        GetParameter =
                            erl_syntax:application(
                                erl_syntax:atom(proplists),
                                erl_syntax:atom(get_value),
                                [
                                    erl_syntax:binary([
                                        erl_syntax:binary_field(
                                            erl_syntax:string(
                                                erlang:binary_to_list(ParameterName)
                                            )
                                        )
                                    ]),
                                    erl_syntax:variable('Headers')
                                ]
                            ),
                        ParameterRequired = maps:get(required, Parameter),
                        {true, #{
                            module => ParameterModule,
                            get => GetParameter,
                            required => ParameterRequired,
                            name => ParameterName,
                            type => header,
                            absent => erl_syntax:atom(undefined)
                        }};
                    cookie ->
                        %% TODO: implement
                        false;
                    path ->
                        GetParameter =
                            erl_syntax:variable(
                                erlang:binary_to_atom(
                                    erf_util:to_pascal_case(ParameterName)
                                )
                            ),
                        {true, #{
                            module => ParameterModule,
                            get => GetParameter,
                            required => true,
                            name => ParameterName,
                            type => path,
                            absent => undefined
                        }};
                    query ->
                        ParameterSchemaType =
                            case ParameterSchema of
                                undefined ->
                                    <<"string">>;
                                _ ->
                                    maps:get(<<"type">>, ParameterSchema, <<"string">>)
                            end,
                        GetParameter =
                            case ParameterSchemaType of
                                <<"array">> ->
                                    ItemsType = maps:get(
                                        <<"type">>,
                                        maps:get(<<"items">>, ParameterSchema, #{}),
                                        <<"string">>
                                    ),
                                    RawValues = erl_syntax:application(
                                        erl_syntax:atom(proplists),
                                        erl_syntax:atom(get_all_values),
                                        [
                                            erl_syntax:binary([
                                                erl_syntax:binary_field(
                                                    erl_syntax:string(
                                                        erlang:binary_to_list(ParameterName)
                                                    )
                                                )
                                            ]),
                                            erl_syntax:variable('QueryParameters')
                                        ]
                                    ),
                                    case ItemsType of
                                        <<"boolean">> ->
                                            erl_syntax:list_comp(
                                                erl_syntax:application(
                                                    erl_syntax:atom(erlang),
                                                    erl_syntax:atom(binary_to_atom),
                                                    [erl_syntax:variable('X')]
                                                ),
                                                [
                                                    erl_syntax:generator(
                                                        erl_syntax:variable('X'),
                                                        RawValues
                                                    )
                                                ]
                                            );
                                        <<"integer">> ->
                                            erl_syntax:list_comp(
                                                erl_syntax:application(
                                                    erl_syntax:atom(erf_util),
                                                    erl_syntax:atom(safe_binary_to_integer),
                                                    [erl_syntax:variable('X')]
                                                ),
                                                [
                                                    erl_syntax:generator(
                                                        erl_syntax:variable('X'),
                                                        RawValues
                                                    )
                                                ]
                                            );
                                        <<"number">> ->
                                            erl_syntax:list_comp(
                                                erl_syntax:application(
                                                    erl_syntax:atom(erf_util),
                                                    erl_syntax:atom(safe_binary_to_number),
                                                    [erl_syntax:variable('X')]
                                                ),
                                                [
                                                    erl_syntax:generator(
                                                        erl_syntax:variable('X'),
                                                        RawValues
                                                    )
                                                ]
                                            );
                                        _ ->
                                            RawValues
                                    end;
                                <<"boolean">> ->
                                    erl_syntax:application(
                                        erl_syntax:atom(erf_util),
                                        erl_syntax:atom(maybe_binary_to_atom),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom(proplists),
                                                erl_syntax:atom(get_value),
                                                [
                                                    erl_syntax:binary([
                                                        erl_syntax:binary_field(
                                                            erl_syntax:string(
                                                                erlang:binary_to_list(ParameterName)
                                                            )
                                                        )
                                                    ]),
                                                    erl_syntax:variable('QueryParameters')
                                                ]
                                            )
                                        ]
                                    );
                                <<"integer">> ->
                                    erl_syntax:application(
                                        erl_syntax:atom(erf_util),
                                        erl_syntax:atom(maybe_safe_binary_to_integer),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom(proplists),
                                                erl_syntax:atom(get_value),
                                                [
                                                    erl_syntax:binary([
                                                        erl_syntax:binary_field(
                                                            erl_syntax:string(
                                                                erlang:binary_to_list(ParameterName)
                                                            )
                                                        )
                                                    ]),
                                                    erl_syntax:variable('QueryParameters')
                                                ]
                                            )
                                        ]
                                    );
                                <<"number">> ->
                                    erl_syntax:application(
                                        erl_syntax:atom(erf_util),
                                        erl_syntax:atom(maybe_safe_binary_to_number),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom(proplists),
                                                erl_syntax:atom(get_value),
                                                [
                                                    erl_syntax:binary([
                                                        erl_syntax:binary_field(
                                                            erl_syntax:string(
                                                                erlang:binary_to_list(ParameterName)
                                                            )
                                                        )
                                                    ]),
                                                    erl_syntax:variable('QueryParameters')
                                                ]
                                            )
                                        ]
                                    );
                                _ ->
                                    erl_syntax:application(
                                        erl_syntax:atom(proplists),
                                        erl_syntax:atom(get_value),
                                        [
                                            erl_syntax:binary([
                                                erl_syntax:binary_field(
                                                    erl_syntax:string(
                                                        erlang:binary_to_list(ParameterName)
                                                    )
                                                )
                                            ]),
                                            erl_syntax:variable('QueryParameters')
                                        ]
                                    )
                            end,
                        ParameterRequired = maps:get(required, Parameter),
                        Absent =
                            case ParameterSchemaType of
                                <<"array">> -> erl_syntax:list([]);
                                _Otherwise -> erl_syntax:atom(undefined)
                            end,
                        {true, #{
                            module => ParameterModule,
                            get => GetParameter,
                            required => ParameterRequired,
                            name => ParameterName,
                            type => query,
                            absent => Absent
                        }}
                end
            end,
            RawParameters
        ),
    IndexedParameters = lists:zip(
        lists:seq(1, erlang:length(FilteredParameters)), FilteredParameters
    ),
    Bindings =
        [
            erl_syntax:match_expr(parameter_variable(Index), GetParameter)
         || {Index, #{get := GetParameter}} <- IndexedParameters
        ],
    Parameters =
        lists:map(
            fun({Index, #{module := ParameterModule, required := ParameterRequired}}) ->
                ParameterVariable = parameter_variable(Index),
                IsValidParameter =
                    erl_syntax:application(
                        erl_syntax:atom(ParameterModule),
                        erl_syntax:atom(is_valid),
                        [ParameterVariable]
                    ),
                case ParameterRequired of
                    true ->
                        IsValidParameter;
                    false ->
                        erl_syntax:infix_expr(
                            erl_syntax:infix_expr(
                                ParameterVariable,
                                erl_syntax:operator('=:='),
                                erl_syntax:atom(undefined)
                            ),
                            erl_syntax:operator('orelse'),
                            IsValidParameter
                        )
                end
            end,
            IndexedParameters
        ),
    Sources =
        erl_syntax:tuple(
            [source(body, undefined, RequestBodyRequired)] ++
                [
                    source(ParameterType, ParameterName, ParameterRequired)
                 || #{
                        type := ParameterType,
                        name := ParameterName,
                        required := ParameterRequired
                    } <- FilteredParameters
                ]
        ),
    Presence =
        erl_syntax:tuple(
            [
                erl_syntax:infix_expr(
                    erl_syntax:variable('Body'),
                    erl_syntax:operator('=/='),
                    erl_syntax:atom(undefined)
                )
            ] ++
                [
                    presence(parameter_variable(Index), Absent)
                 || {Index, #{absent := Absent}} <- IndexedParameters
                ]
        ),
    IsValidRequest =
        erl_syntax:application(
            erl_syntax:atom('ndto_validation'),
            erl_syntax:atom('andalso'),
            [
                erl_syntax:list([
                    erl_syntax:tuple([
                        erl_syntax:fun_expr([
                            erl_syntax:clause(
                                none,
                                [Condition]
                            )
                        ]),
                        erl_syntax:list([])
                    ])
                 || Condition <- [RequestBody | Parameters]
                ])
            ]
        ),
    #{
        bindings => Bindings,
        is_valid_request => IsValidRequest,
        sources => Sources,
        presence => Presence
    }.

-spec parameter_variable(Index) -> Variable when
    Index :: pos_integer(),
    Variable :: erl_syntax:syntaxTree().
%% The underscore keeps these apart from path parameter variables, which come
%% from `erf_util:to_pascal_case/1' and therefore never hold one.
parameter_variable(Index) ->
    erl_syntax:variable(
        erlang:list_to_atom("Erf_Param" ++ erlang:integer_to_list(Index))
    ).

-spec source(Type, Name, Required) -> Source when
    Type :: erf_validation:in(),
    Name :: erf_parser:parameter_name() | undefined,
    Required :: boolean(),
    Source :: erl_syntax:syntaxTree().
%% A literal the compiler lifts into the module's constant pool, so it costs
%% nothing until the failure branch reads it.
source(Type, Name, Required) ->
    NameAST =
        case Name of
            undefined ->
                erl_syntax:atom(undefined);
            _Name ->
                erl_syntax:binary([
                    erl_syntax:binary_field(
                        erl_syntax:string(erlang:binary_to_list(Name))
                    )
                ])
        end,
    erl_syntax:tuple([erl_syntax:atom(Type), NameAST, erl_syntax:atom(Required)]).

-spec presence(Variable, Absent) -> Presence when
    Variable :: erl_syntax:syntaxTree(),
    Absent :: erl_syntax:syntaxTree() | undefined,
    Presence :: erl_syntax:syntaxTree().
%% A path parameter is present whenever its clause matched, so it needs no test.
presence(_Variable, undefined) ->
    erl_syntax:atom(true);
presence(Variable, Absent) ->
    erl_syntax:infix_expr(Variable, erl_syntax:operator('=/='), Absent).

-spec load_binary(ModuleName, Bin) -> Result when
    ModuleName :: atom(),
    Bin :: binary(),
    Result :: ok | {error, Reason},
    Reason :: term().
load_binary(ModuleName, Bin) ->
    case
        code:load_binary(
            ModuleName, erlang:atom_to_list(ModuleName) ++ ".erl", Bin
        )
    of
        {module, ModuleName} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec postprocess(Request, RawResponse) -> Response when
    Request :: erf:request(),
    RawResponse :: erf:response(),
    Response :: erf:response().
postprocess(_Request, {_Status, _Headers, {file, _Path}} = Response) ->
    Response;
postprocess(_Request, {_Status, _Headers, {stream, _Producer}} = Response) ->
    Response;
postprocess(_Request, {Status, RawHeaders, RawBody}) ->
    ContentTypeHeader = string:casefold(<<"content-type">>),
    case proplists:get_value(ContentTypeHeader, RawHeaders, undefined) of
        undefined ->
            case RawBody of
                undefined ->
                    {Status, RawHeaders, undefined};
                _RawBody ->
                    try iolist_to_binary(json:encode(RawBody)) of
                        EncodedBody ->
                            Headers = [{ContentTypeHeader, <<"application/json">>} | RawHeaders],
                            {Status, Headers, EncodedBody}
                    catch
                        error:_ ->
                            {Status, [{ContentTypeHeader, <<"text/plain">>} | RawHeaders], RawBody}
                    end
            end;
        <<"application/json">> ->
            try iolist_to_binary(json:encode(RawBody)) of
                EncodedBody ->
                    {Status, RawHeaders, EncodedBody}
            catch
                error:_ ->
                    % TODO: handle error
                    {500, [{ContentTypeHeader, <<"text/plain">>}], <<"Internal Server Error">>}
            end;
        _Otherwise ->
            {Status, RawHeaders, RawBody}
    end.

-spec preprocess(RawRequest) -> Result when
    RawRequest :: erf:request(),
    Result :: {ok, Request} | {error, Reason},
    Request :: erf:request(),
    Reason :: term().
preprocess(RawRequest) ->
    Headers = maps:get(headers, RawRequest, []),
    ContentTypeHeader = string:casefold(<<"content-type">>),
    RawBody = maps:get(body, RawRequest, undefined),
    case proplists:get_value(ContentTypeHeader, Headers, undefined) of
        <<"application/json">> ->
            case RawBody of
                NonEmptyBinary when is_binary(NonEmptyBinary), byte_size(NonEmptyBinary) > 0 ->
                    try json:decode(RawBody) of
                        Body ->
                            {ok, RawRequest#{body => Body}}
                    catch
                        error:Reason ->
                            {error, {cannot_decode_body, Reason}}
                    end;
                _EmptyBody ->
                    % Content-Type describes content that is not there, so it says nothing.
                    {ok, RawRequest#{body => undefined}}
            end;
        _ContentType ->
            {ok, RawRequest}
    end.
