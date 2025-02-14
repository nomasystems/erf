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
-type generator_opts() :: #{callback := callback(), static_routes := [erf:static_route()]}.

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
    case preprocess(RawRequest) of
        {ok, Request} ->
            {InitialResponse, InitialRequest} =
                case apply_preprocess_middlewares(Request, PreProcessMiddlewares) of
                    {stop, PreprocessResponse, PreprocessRequest} ->
                        {PreprocessResponse, PreprocessRequest};
                    PreprocessRequest ->
                        {RouterMod:handle(PreprocessRequest), PreprocessRequest}
                end,
            Response = apply_postprocess_middlewares(
                InitialRequest, InitialResponse, PostProcessMiddlewares
            ),
            postprocess(InitialRequest, Response);
        {error, _Reason} ->
            ContentTypeHeader = string:casefold(<<"content-type">>),
            {ok, ErrorBody} = njson:encode(#{
                <<"title">> => <<"Bad Request">>,
                <<"status">> => 400,
                <<"detail">> => <<"Failed to read request">>
            }),
            {400, [{ContentTypeHeader, <<"application/json">>}], ErrorBody}
    end.

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
                    IsValidRequestAST = is_valid_request(
                        Parameters,
                        Request
                    ),

                    erl_syntax:clause(
                        [
                            erl_syntax:match_expr(
                                erl_syntax:variable('Request0'),
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
                            ),
                            erl_syntax:match_expr(
                                erl_syntax:variable('IsValidRequest'),
                                IsValidRequestAST
                            ),
                            erl_syntax:match_expr(
                                erl_syntax:variable('Request'),
                                erl_syntax:map_expr(
                                    erl_syntax:variable('Request0'),
                                    [
                                        erl_syntax:map_field_assoc(
                                            erl_syntax:atom('path_parameters'),
                                            erl_syntax:variable('PathParameters')
                                        )
                                    ]
                                )
                            ),
                            erl_syntax:case_expr(
                                erl_syntax:variable('IsValidRequest'),
                                [
                                    erl_syntax:clause(
                                        [erl_syntax:atom(true)],
                                        none,
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom(Callback),
                                                erl_syntax:atom(
                                                    erlang:binary_to_atom(
                                                        erf_util:to_snake_case(
                                                            maps:get(id, Operation)
                                                        ),
                                                        utf8
                                                    )
                                                ),
                                                [erl_syntax:variable('Request')]
                                            )
                                        ]
                                    ),
                                    erl_syntax:clause(
                                        [
                                            erl_syntax:tuple([
                                                erl_syntax:atom(false),
                                                erl_syntax:variable('Reason')
                                            ])
                                        ],
                                        none,
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom(erf_util),
                                                erl_syntax:atom(handle_invalid_request),
                                                [
                                                    erl_syntax:variable('Request'),
                                                    erl_syntax:variable('Reason')
                                                ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                end,
                maps:get(operations, Endpoint, [])
            ),
            NotAllowedMethod =
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
                                erl_syntax:list([]),
                                erl_syntax:atom(undefined)
                            ]
                        )
                    ]
                ),

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

-spec is_valid_request(Parameters, Request) -> Result when
    Parameters :: [erf_parser:parameter()],
    Request :: erf_parser:request(),
    Result :: erl_syntax:syntaxTree().
is_valid_request(RawParameters, Request) ->
    RawRequestBody = maps:get(body, Request),
    RequestBodyRef = maps:get(ref, RawRequestBody),
    RequestBodyModule =
        erlang:binary_to_atom(erf_util:to_snake_case(RequestBodyRef)),
    RequestBodyIsValid =
        erl_syntax:application(
            erl_syntax:atom(RequestBodyModule),
            erl_syntax:atom(is_valid),
            [erl_syntax:variable('Body')]
        ),
    RequestBody =
        case maps:get(required, RawRequestBody) of
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
                            required => ParameterRequired
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
                            required => true
                        }};
                    query ->
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
                                    erl_syntax:variable('QueryParameters')
                                ]
                            ),
                        ParameterRequired = maps:get(required, Parameter),
                        {true, #{
                            module => ParameterModule,
                            get => GetParameter,
                            required => ParameterRequired
                        }}
                end
            end,
            RawParameters
        ),
    Parameters =
        lists:map(
            fun(#{module := ParameterModule, get := GetParameter, required := ParameterRequired}) ->
                IsValidParameter =
                    erl_syntax:application(
                        erl_syntax:atom(ParameterModule),
                        erl_syntax:atom(is_valid),
                        [GetParameter]
                    ),
                OptionalParameter =
                    erl_syntax:infix_expr(
                        GetParameter,
                        erl_syntax:operator('=:='),
                        erl_syntax:atom(undefined)
                    ),
                case ParameterRequired of
                    true ->
                        IsValidParameter;
                    false ->
                        erl_syntax:infix_expr(
                            OptionalParameter,
                            erl_syntax:operator('orelse'),
                            IsValidParameter
                        )
                end
            end,
            FilteredParameters
        ),
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
    ).

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
postprocess(_Request, {Status, RawHeaders, RawBody}) ->
    ContentTypeHeader = string:casefold(<<"content-type">>),
    case proplists:get_value(ContentTypeHeader, RawHeaders, undefined) of
        undefined ->
            case RawBody of
                undefined ->
                    {Status, RawHeaders, []};
                _RawBody ->
                    case njson:encode(RawBody) of
                        {ok, EncodedBody} ->
                            Headers = [{ContentTypeHeader, <<"application/json">>} | RawHeaders],
                            {Status, Headers, EncodedBody};
                        {error, _Reason} ->
                            {Status, [{ContentTypeHeader, <<"text/plain">>} | RawHeaders], RawBody}
                    end
            end;
        <<"application/json">> ->
            case njson:encode(RawBody) of
                {ok, EncodedBody} ->
                    {Status, RawHeaders, EncodedBody};
                {error, _Reason} ->
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
                    case njson:decode(RawBody) of
                        {ok, Body} ->
                            {ok, RawRequest#{body => Body}};
                        {error, Reason} ->
                            {error, {cannot_decode_body, Reason}}
                    end;
                _RawBody ->
                    {error, {cannot_decode_body, invalid_json}}
            end;
        _ContentType ->
            {ok, RawRequest}
    end.
