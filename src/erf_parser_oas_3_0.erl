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
-module(erf_parser_oas_3_0).

%%% BEHAVIOURS
-behaviour(erf_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/1
]).

%%% RECORDS
-record(ctx, {
    base_path :: binary(),
    namespace = <<"">> :: binary(),
    oas :: oas()
}).

%%% TYPES
-type oas() :: term().

%%% MACROS
-define(METHODS, [
    <<"get">>,
    <<"put">>,
    <<"post">>,
    <<"delete">>,
    <<"options">>,
    <<"head">>,
    <<"patch">>,
    <<"trace">>
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, API} | {error, Reason},
    API :: erf:api(),
    Reason :: term().
%% @doc Parses an OpenAPI Specification 3.0 file into an API AST.
parse(SpecPath) ->
    case file:read_file(SpecPath) of
        {ok, Bin} ->
            OAS = njson:decode(Bin),
            case oas_3_0:is_valid(OAS) of
                true ->
                    BasePath = filename:dirname(SpecPath),
                    {ok, parse_api(OAS, #ctx{base_path = BasePath, oas = OAS})};
                false ->
                    {error, {invalid_spec, <<"Invalid OpenAPI Specification 3.0">>}}
            end;
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec get(Keys, OAS) -> Result when
    Keys :: [binary()],
    OAS :: oas(),
    Result :: term().
get([], OAS) ->
    OAS;
get([Key | Keys], OAS) ->
    get(Keys, maps:get(Key, OAS)).

-spec parse_api(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: #ctx{},
    Result :: erf:api().
parse_api(OAS, CTX) ->
    Name = parse_name(OAS),
    Version = parse_version(OAS),
    {RawEndpoints, RawSchemas} = lists:foldl(
        fun({Path, RawEndpoint}, {AccEndpoints, AccSchemas}) ->
            {Endpoint, EndpointSchemas} = parse_endpoint(Path, RawEndpoint, CTX),
            {[Endpoint | AccEndpoints], EndpointSchemas ++ AccSchemas}
        end,
        {[], []},
        maps:to_list(maps:get(<<"paths">>, OAS))
    ),
    Endpoints = lists:reverse(RawEndpoints),
    Schemas = maps:from_list(RawSchemas),
    #{
        name => Name,
        version => Version,
        endpoints => Endpoints,
        schemas => Schemas
    }.

-spec parse_endpoint(Path, Endpoint, CTX) -> Result when
    Path :: binary(),
    Endpoint :: oas(),
    CTX :: #ctx{},
    Result :: {Endpoint, Schemas},
    Endpoint :: erf_parser:endpoint(),
    Schemas :: [{erf_parser:ref(), erf_parser:schema()}].
parse_endpoint(Path, RawEndpoint, #ctx{namespace = NS} = CTX) ->
    NewCTX = CTX#ctx{namespace = erf_util:to_snake_case(<<NS/binary, Path/binary>>)},

    RawParameters =
        lists:map(
            fun(RawParameter) ->
                parse_parameter(RawParameter, NewCTX)
            end,
            maps:get(<<"parameters">>, RawEndpoint, [])
        ),
    {Parameters, ParametersSchemas} = lists:unzip(RawParameters),

    RawOperations =
        lists:filtermap(
            fun(Method) ->
                case maps:get(Method, RawEndpoint, undefined) of
                    undefined ->
                        false;
                    Operation ->
                        {true, {Method, Operation}}
                end
            end,
            ?METHODS
        ),
    {Operations, RawOperationsSchemas} = lists:unzip(
        lists:map(
            fun({Method, RawOperation}) ->
                parse_operation(Path, Method, RawOperation, CTX)
            end,
            RawOperations
        )
    ),
    OperationsSchemas = lists:flatten(RawOperationsSchemas),

    Endpoint = #{
        path => Path,
        parameters => Parameters,
        operations => Operations
    },
    Schemas = ParametersSchemas ++ OperationsSchemas,
    {Endpoint, Schemas}.

-spec parse_method(Method) -> Result when
    Method :: binary(),
    Result :: erf_parser:method().
parse_method(<<"get">>) ->
    get;
parse_method(<<"post">>) ->
    post;
parse_method(<<"put">>) ->
    put;
parse_method(<<"delete">>) ->
    delete;
parse_method(<<"patch">>) ->
    patch;
parse_method(<<"head">>) ->
    head;
parse_method(<<"options">>) ->
    options;
parse_method(<<"trace">>) ->
    trace;
parse_method(<<"connect">>) ->
    connect.

-spec parse_name(Val) -> Result when
    Val :: oas(),
    Result :: binary().
parse_name(#{<<"info">> := #{<<"title">> := Name}}) ->
    Name.

-spec parse_operation(Path, Method, RawOperation, CTX) -> Result when
    Path :: binary(),
    Method :: binary(),
    RawOperation :: oas(),
    CTX :: #ctx{},
    Result :: {Operation, Schemas},
    Operation :: erf_parser:operation(),
    Schemas :: [{erf_parser:ref(), erf_parser:schema()}].
parse_operation(
    Path, RawMethod, #{<<"responses">> := RawResponses} = RawOperation, CTX
) ->
    OperationId =
        case maps:get(<<"operationId">>, RawOperation, undefined) of
            undefined ->
                erf_util:to_snake_case(<<Path/binary, "_", RawMethod/binary>>);
            RawOperationId ->
                erf_util:to_snake_case(RawOperationId)
        end,
    NewCTX = CTX#ctx{namespace = OperationId},
    Method = parse_method(RawMethod),

    RawParameters =
        lists:map(
            fun(RawParameter) ->
                parse_parameter(RawParameter, NewCTX)
            end,
            maps:get(<<"parameters">>, RawOperation, [])
        ),
    {Parameters, ParametersSchemas} = lists:unzip(RawParameters),

    RequestBodyRef = erf_util:to_snake_case(<<(NewCTX#ctx.namespace)/binary, "_request_body">>),
    RawRequestBody = maps:get(<<"requestBody">>, RawOperation, undefined),
    RequestBodySchema = parse_request_body(RawRequestBody, NewCTX),

    ResponsesList =
        lists:map(
            fun({RawStatusCode, RawResponse}) ->
                StatusCode =
                    case RawStatusCode of
                        <<"default">> ->
                            '*';
                        _RawStatusCode ->
                            erlang:binary_to_integer(RawStatusCode)
                    end,
                Body = parse_response_body(RawResponse, NewCTX),
                RawRef =
                    case StatusCode of
                        '*' ->
                            <<"default">>;
                        _StatusCode ->
                            erlang:integer_to_binary(StatusCode)
                    end,
                Ref = erf_util:to_snake_case(<<
                    (NewCTX#ctx.namespace)/binary, "_response_body_", RawRef/binary
                >>),
                #{status_code => StatusCode, schema => Body, ref => Ref}
            end,
            maps:to_list(RawResponses)
        ),

    {ResponsesSchemas, Responses} =
        lists:foldl(
            fun(
                #{status_code := StatusCode, schema := Schema, ref := Ref},
                {SchemasAcc, ResponsesAcc}
            ) ->
                {[{Ref, Schema} | SchemasAcc], ResponsesAcc#{StatusCode => Ref}}
            end,
            {[], #{}},
            ResponsesList
        ),

    Operation = #{
        id => OperationId,
        method => Method,
        parameters => Parameters,
        request_body => RequestBodyRef,
        responses => Responses
    },
    Schemas = lists:flatten([
        [
            {RequestBodyRef, RequestBodySchema}
            | ResponsesSchemas
        ]
        | ParametersSchemas
    ]),
    {Operation, Schemas}.

-spec parse_parameter(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: #ctx{},
    Result :: {Parameter, Schema},
    Parameter :: erf_parser:parameter(),
    Schema :: {erf_parser:ref(), erf_parser:schema()}.
parse_parameter(#{<<"$ref">> := Ref}, CTX) ->
    {Parameter, NewCTX} = resolve_ref(Ref, CTX),
    parse_parameter(Parameter, NewCTX);
parse_parameter(#{<<"content">> := Content} = Parameter, #ctx{namespace = NS} = CTX) ->
    ParameterType =
        case maps:get(<<"in">>, Parameter) of
            <<"query">> ->
                query;
            <<"header">> ->
                header;
            <<"path">> ->
                path;
            <<"cookie">> ->
                cookie
        end,
    DefaultRequired =
        case ParameterType of
            path ->
                true;
            _Type ->
                false
        end,
    Required = maps:get(<<"required">>, Parameter, DefaultRequired),
    ParameterName = maps:get(<<"name">>, Parameter),
    ParameterRef = erf_util:to_snake_case(<<NS/binary, "_", ParameterName/binary>>),
    Parameter = #{
        ref => ParameterRef,
        name => ParameterName,
        type => ParameterType
    },
    ParameterSchema = #{
        <<"anyOf">> =>
            lists:map(
                fun({_MediaType, #{<<"schema">> := RawSchema}}) ->
                    Schema = parse_schema(RawSchema, CTX),
                    Schema#{<<"nullable">> => not Required}
                end,
                maps:to_list(Content)
            )
    },
    {Parameter, {ParameterRef, ParameterSchema}};
parse_parameter(#{<<"schema">> := RawSchema} = RawParameter, #ctx{namespace = NS} = _CTX) ->
    ParameterType =
        case maps:get(<<"in">>, RawParameter) of
            <<"query">> ->
                query;
            <<"header">> ->
                header;
            <<"path">> ->
                path;
            <<"cookie">> ->
                cookie
        end,
    DefaultRequired =
        case ParameterType of
            path ->
                true;
            _Type ->
                false
        end,
    Required = maps:get(<<"required">>, RawParameter, DefaultRequired),
    ParameterName = maps:get(<<"name">>, RawParameter),
    ParameterRef = erf_util:to_snake_case(<<NS/binary, "_", ParameterName/binary>>),
    Parameter = #{
        ref => ParameterRef,
        name => ParameterName,
        type => ParameterType
    },
    ParameterSchema = RawSchema#{<<"nullable">> => not Required},
    {Parameter, {ParameterRef, ParameterSchema}}.

-spec resolve_ref(Ref, CTX) -> Result when
    Ref :: binary(),
    CTX :: #ctx{},
    Result :: {oas(), #ctx{}}.
resolve_ref(Ref, #ctx{base_path = BasePath, oas = OAS} = CTX) ->
    [FilePath, ElementPath] = binary:split(Ref, <<"#">>, [global]),
    {NewOAS, NewBasePath} =
        case FilePath of
            <<>> ->
                {OAS, BasePath};
            _FilePath ->
                AbsPath = filename:join(BasePath, FilePath),
                case file:read_file(AbsPath) of
                    {ok, Bin} ->
                        {njson:decode(Bin), filename:dirname(AbsPath)};
                    {error, Reason} ->
                        erlang:error({invalid_spec, Reason})
                end
        end,
    [<<>> | Steps] = binary:split(ElementPath, <<"/">>, [global]),
    Component = get(Steps, NewOAS),
    {Component, CTX#ctx{base_path = NewBasePath, oas = NewOAS}}.

-spec parse_request_body(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: #ctx{},
    Result :: erf_parser:schema().
parse_request_body(#{<<"$ref">> := Ref}, CTX) ->
    {RequestBody, NewCTX} = resolve_ref(Ref, CTX),
    parse_request_body(RequestBody, NewCTX);
parse_request_body(#{<<"content">> := Content} = ReqBody, CTX) ->
    Required = maps:get(<<"required">>, ReqBody, false),
    #{
        <<"anyOf">> =>
            lists:map(
                fun({_MediaType, #{<<"schema">> := RawSchema}}) ->
                    Schema = parse_schema(RawSchema, CTX),
                    Schema#{<<"nullable">> => not Required}
                end,
                maps:to_list(Content)
            )
    };
parse_request_body(_ReqBody, _CTX) ->
    undefined.

-spec parse_response_body(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: #ctx{},
    Result :: erf_parser:schema().
parse_response_body(#{<<"$ref">> := Ref}, CTX) ->
    {Response, NewCTX} = resolve_ref(Ref, CTX),
    parse_response_body(Response, NewCTX);
parse_response_body(#{<<"content">> := Content}, CTX) ->
    #{
        <<"anyOf">> =>
            lists:map(
                fun({_MediaType, #{<<"schema">> := Schema}}) ->
                    parse_schema(Schema, CTX)
                end,
                maps:to_list(Content)
            )
    };
parse_response_body(_Response, _CTX) ->
    undefined.

-spec parse_schema(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: #ctx{},
    Result :: erf_parser:schema().
parse_schema(#{<<"$ref">> := Ref}, CTX) ->
    {Schema, NewCTX} = resolve_ref(Ref, CTX),
    parse_schema(Schema, NewCTX);
parse_schema(#{<<"items">> := Items} = Schema, CTX) ->
    Schema#{
        <<"items">> => parse_schema(Items, CTX)
    };
parse_schema(#{<<"properties">> := Properties} = Schema, CTX) ->
    RawParsedSchema = Schema#{
        <<"properties">> => maps:from_list(
            lists:map(
                fun({Name, Property}) ->
                    {Name, parse_schema(Property, CTX)}
                end,
                maps:to_list(Properties)
            )
        )
    },
    AdditionalProperties =
        case maps:get(<<"additionalProperties">>, Schema, undefined) of
            Object when is_map(Object) ->
                parse_schema(Object, CTX);
            Otherwise ->
                Otherwise
        end,
    case AdditionalProperties of
        undefined ->
            RawParsedSchema;
        _AdditionalProperties ->
            RawParsedSchema#{
                <<"additionalProperties">> => AdditionalProperties
            }
    end;
parse_schema(#{<<"allOf">> := AllOf} = Schema, CTX) ->
    Schema#{
        <<"allOf">> => lists:map(
            fun(RawSchema) ->
                parse_schema(RawSchema, CTX)
            end,
            AllOf
        )
    };
parse_schema(#{<<"oneOf">> := OneOf} = Schema, CTX) ->
    Schema#{
        <<"oneOf">> => lists:map(
            fun(RawSchema) ->
                parse_schema(RawSchema, CTX)
            end,
            OneOf
        )
    };
parse_schema(#{<<"anyOf">> := AnyOf} = Schema, CTX) ->
    Schema#{
        <<"anyOf">> => lists:map(
            fun(RawSchema) ->
                parse_schema(RawSchema, CTX)
            end,
            AnyOf
        )
    };
parse_schema(#{<<"not">> := Not} = Schema, CTX) ->
    Schema#{
        <<"not">> => parse_schema(Not, CTX)
    };
parse_schema(Schema, _CTX) ->
    Schema.

-spec parse_version(OAS) -> Version when
    OAS :: oas(),
    Version :: binary().
parse_version(#{<<"info">> := #{<<"version">> := Version}}) ->
    Version.
