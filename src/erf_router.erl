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

%%% INCLUDE FILES
-include("erf_generator.hrl").

%%% EXTERNAL EXPORTS
-export([
    generate/2,
    load/1
]).

%%% ELLI HANDLER EXPORTS
-export([
    handle/2,
    handle_event/3
]).

%%% TYPES
-type t() :: erl_syntax:syntaxTree().
-type callback() :: module().
-type middlewares_conf() :: proplists:proplist().
-type opts() :: #{callback := callback()}.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(API, Opts) -> Result when
    API :: erf:api(),
    Opts :: opts(),
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

%%%-----------------------------------------------------------------------------
%%% ELLI HANDLER EXPORTS
%%%-----------------------------------------------------------------------------
-spec handle(InitialRequest, MiddlewaresConf) -> Result when
    InitialRequest :: elli:req(),
    MiddlewaresConf :: middlewares_conf(),
    Result :: elli:response().
%% @doc Handles an HTTP request.
%% @private
handle(ElliRequest, MiddlewaresConf) ->
    InitialRequest = preprocess_request(ElliRequest),
    Request = lists:foldl(
        fun(Middleware, ReqAcc) ->
            Middleware:preprocess(ReqAcc)
        end,
        InitialRequest,
        proplists:get_value(preprocess_middlewares, MiddlewaresConf, [])
    ),
    Router = proplists:get_value(router, MiddlewaresConf),
    InitialResponse = Router:handle(Request),
    Response = lists:foldl(
        fun(Middleware, RespAcc) ->
            Middleware:postprocess(Request, RespAcc)
        end,
        InitialResponse,
        proplists:get_value(postprocess_middlewares, MiddlewaresConf, [])
    ),
    postprocess_response(Response).

-spec handle_event(Event, Data, MiddlewaresConf) -> ok when
    Event :: atom(),
    Data :: term(),
    MiddlewaresConf :: proplists:proplist().
%% @doc Handles an elli event.
%% @private
handle_event(_Event, _Data, _MiddlewaresConf) ->
    % TODO: address the event system
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec chain_conditions(FunCalls, Operator) -> Result when
    FunCalls :: [erl_syntax:syntaxTree()],
    Operator :: 'andalso',
    Result :: erl_syntax:syntaxTree().
chain_conditions(FunCalls, 'andalso' = Operator) ->
    chain_conditions(FunCalls, Operator, erl_syntax:atom(true)).

chain_conditions([], _Operator, Acc) ->
    Acc;
chain_conditions([FunCall | Rest], Operator, Acc) ->
    NewAcc = erl_syntax:infix_expr(
        Acc,
        erl_syntax:operator(Operator),
        FunCall
    ),
    chain_conditions(Rest, Operator, NewAcc).

-spec handle_ast(API, Opts) -> Result when
    API :: erf:api(),
    Opts :: opts(),
    Result :: t().
handle_ast(API, Opts) ->
    Callback = maps:get(callback, Opts),
    Clauses = lists:flatmap(
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
                        (Part) ->
                            erl_syntax:binary([
                                erl_syntax:binary_field(
                                    erl_syntax:string(erlang:binary_to_list(Part))
                                )
                            ])
                    end,
                    RawPath
                )
            ),
            EndpointParameters = maps:get(parameters, Endpoint),
            lists:map(
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
                    RequestBody = maps:get(request_body, Operation),

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
                        RequestBody
                    ),

                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                Path,
                                Method,
                                erl_syntax:variable('Headers'),
                                erl_syntax:variable('QueryParameters'),
                                erl_syntax:variable('Body')
                            ])
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
                                                [
                                                    erl_syntax:variable('PathParameters'),
                                                    erl_syntax:variable('Headers'),
                                                    erl_syntax:variable('QueryParameters'),
                                                    erl_syntax:variable('Body')
                                                ]
                                            )
                                        ]
                                    ),
                                    erl_syntax:clause(
                                        [erl_syntax:atom(false)],
                                        none,
                                        [
                                            erl_syntax:tuple(
                                                [
                                                    erl_syntax:integer(400),
                                                    erl_syntax:list([]),
                                                    erl_syntax:atom(undefined)
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
            )
        end,
        maps:get(endpoints, API, [])
    ),
    erl_syntax:function(
        erl_syntax:atom(handle),
        Clauses
    ).

-spec is_valid_request(Parameters, RequestBody) -> Result when
    Parameters :: [erf_parser:parameter()],
    RequestBody :: erf_parser:ref(),
    Result :: erl_syntax:syntaxTree().
is_valid_request(RawParameters, RequestBody) ->
    Body =
        case RequestBody of
            undefined ->
                erl_syntax:atom(true);
            _RequestBody ->
                RequestBodyModule = erlang:binary_to_atom(erf_util:to_snake_case(RequestBody)),
                erl_syntax:application(
                    erl_syntax:atom(RequestBodyModule),
                    erl_syntax:atom(is_valid),
                    [erl_syntax:variable('Body')]
                )
        end,
    Parameters = lists:filtermap(
        fun(Parameter) ->
            ParameterModule = erlang:binary_to_atom(maps:get(ref, Parameter)),
            ParameterName = maps:get(name, Parameter),
            ParameterType = maps:get(type, Parameter),
            case ParameterType of
                header ->
                    {
                        true,
                        erl_syntax:application(
                            erl_syntax:atom(ParameterModule),
                            erl_syntax:atom(is_valid),
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
                                        erl_syntax:variable('Headers')
                                    ]
                                )
                            ]
                        )
                    };
                cookie ->
                    %% TODO: implement
                    false;
                path ->
                    {
                        true,
                        erl_syntax:application(
                            erl_syntax:atom(ParameterModule),
                            erl_syntax:atom(is_valid),
                            [
                                erl_syntax:variable(
                                    erlang:binary_to_atom(
                                        erf_util:to_pascal_case(ParameterName)
                                    )
                                )
                            ]
                        )
                    };
                query ->
                    {
                        true,
                        erl_syntax:application(
                            erl_syntax:atom(ParameterModule),
                            erl_syntax:atom(is_valid),
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
                        )
                    }
            end
        end,
        RawParameters
    ),
    chain_conditions([Body | Parameters], 'andalso').

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

-spec preprocess_request(Req) -> Request when
    Req :: elli:req(),
    Request :: erf:request().
preprocess_request(Req) ->
    Path = elli_request:path(Req),
    Method = erlang:list_to_atom(string:lowercase(erlang:atom_to_list(elli_request:method(Req)))),
    Headers = elli_request:headers(Req),
    QueryParameters = elli_request:get_args_decoded(Req),
    Body = njson:decode(elli_request:body(Req)),
    {Path, Method, Headers, QueryParameters, Body}.

-spec postprocess_response(Response) -> Resp when
    Response :: erf:response(),
    Resp :: elli_handler:result().
postprocess_response(Response) ->
    {Status, Headers, Body} = Response,
    {Status, Headers, njson:encode(Body)}.
