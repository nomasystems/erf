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

%% @doc An OpenAPI Specification 3.0 <code>erf_parser</code>.
-module(erf_oas_3_0).

%%% BEHAVIOURS
-behaviour(erf_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/1
]).

%%% RECORDS
-record(ctx, {
    base_path :: binary(),
    namespace :: binary(),
    resolved :: [erf_parser:ref()],
    spec :: oas()
}).

%%% TYPES
-type ctx() :: #ctx{}.
-type oas() :: njson:t().

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
    case read_spec(SpecPath) of
        {ok, BinSpec} ->
            case deserialize_spec(BinSpec) of
                {ok, OAS} ->
                    case oas_3_0:is_valid(OAS) of
                        true ->
                            CTX = #ctx{
                                base_path = SpecPath,
                                namespace = filename:rootname(filename:basename(SpecPath)),
                                resolved = [],
                                spec = OAS
                            },
                            {ok, parse_api(OAS, CTX)};
                        false ->
                            {error, {invalid_spec, <<"Invalid OpenAPI Specification 3.0">>}}
                    end;
                {error, Reason} ->
                    {error, {invalid_spec, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec deserialize_spec(Bin) -> Result when
    Bin :: binary(),
    Result :: {ok, map()} | {error, Reason},
    Reason :: term().
deserialize_spec(Bin) ->
    try
        Data = njson:decode(Bin),
        {ok, Data}
    catch
        _Error:Reason ->
            {error, {invalid_json, Reason}}
    end.

-spec get(Keys, Spec) -> Result when
    Keys :: [binary()],
    Spec :: map(),
    Result :: term().
get([], Spec) ->
    Spec;
get([Key | Keys], Spec) ->
    get(Keys, maps:get(Key, Spec)).

-spec parse_api(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: ctx(),
    Result :: erf:api().
parse_api(OAS, CTX) ->
    Name = parse_name(OAS),
    Version = parse_version(OAS),
    {RawEndpoints, RawSchemas, _NewCTX} = lists:foldl(
        fun({Path, RawEndpoint}, {EndpointsAcc, SchemasAcc, CTXAcc}) ->
            {Endpoint, EndpointSchemas, NewCTX} = parse_endpoint(Path, RawEndpoint, CTXAcc),
            {[Endpoint | EndpointsAcc], SchemasAcc ++ EndpointSchemas, NewCTX}
        end,
        {[], [], CTX},
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

-spec parse_endpoint(Path, RawEndpoint, CTX) -> Result when
    Path :: binary(),
    RawEndpoint :: oas(),
    CTX :: ctx(),
    Result :: {Endpoint, Schemas, NewCTX},
    Endpoint :: erf_parser:endpoint(),
    Schemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_endpoint(Path, RawEndpoint, #ctx{namespace = Namespace} = CTX) ->
    EndpointNamespace = erf_util:to_snake_case(<<Namespace/binary, Path/binary>>),
    {Parameters, ParametersSchemas, ParametersCTX} =
        lists:foldl(
            fun(RawParameter, {ParametersAcc, ParametersExtraSchemasAcc, ParametersCTXAcc}) ->
                {Parameter, ParameterExtraSchemas, ParameterCTX} = parse_parameter(
                    RawParameter, ParametersCTXAcc
                ),
                {
                    [Parameter | ParametersAcc],
                    ParameterExtraSchemas ++ ParametersExtraSchemasAcc,
                    ParameterCTX
                }
            end,
            {[], [], CTX#ctx{namespace = EndpointNamespace}},
            maps:get(<<"parameters">>, RawEndpoint, [])
        ),

    RawOperations = lists:reverse(
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
        )
    ),
    {Operations, OperationsSchemas, OperationsCTX} =
        lists:foldl(
            fun(
                {Method, RawOperation}, {OperationsAcc, OperationsExtraSchemasAcc, OperationsCTXAcc}
            ) ->
                {Operation, Schemas, OperationCTX} = parse_operation(
                    Path, Method, RawOperation, OperationsCTXAcc
                ),
                {[Operation | OperationsAcc], Schemas ++ OperationsExtraSchemasAcc, OperationCTX}
            end,
            {[], [], ParametersCTX#ctx{namespace = Namespace}},
            RawOperations
        ),

    Endpoint = #{
        path => Path,
        parameters => Parameters,
        operations => Operations
    },
    Schemas = ParametersSchemas ++ OperationsSchemas,
    {Endpoint, Schemas, OperationsCTX#ctx{namespace = Namespace}}.

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
    CTX :: ctx(),
    Result :: {Operation, Schemas, NewCTX},
    Operation :: erf_parser:operation(),
    Schemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_operation(
    Path,
    RawMethod,
    #{<<"responses">> := RawResponses} = RawOperation,
    #ctx{namespace = Namespace} = CTX
) ->
    OperationId =
        case maps:get(<<"operationId">>, RawOperation, undefined) of
            undefined ->
                erf_util:to_snake_case(<<Path/binary, "_", RawMethod/binary>>);
            RawOperationId ->
                erf_util:to_snake_case(RawOperationId)
        end,
    NewCTX = CTX#ctx{namespace = <<Namespace/binary, "_", OperationId/binary>>},
    Method = parse_method(RawMethod),

    {Parameters, ParametersSchemas, ParametersCTX} =
        lists:foldl(
            fun(RawParameter, {ParametersAcc, ParametersExtraSchemasAcc, ParametersCTXAcc}) ->
                {Parameter, ParameterExtraSchemas, ParameterCTX} = parse_parameter(
                    RawParameter, ParametersCTXAcc
                ),
                {
                    [Parameter | ParametersAcc],
                    ParameterExtraSchemas ++ ParametersExtraSchemasAcc,
                    ParameterCTX
                }
            end,
            {[], [], NewCTX},
            maps:get(<<"parameters">>, RawOperation, [])
        ),

    RequestBodyRef = erf_util:to_snake_case(<<(NewCTX#ctx.namespace)/binary, "_request_body">>),
    RawRequestBody = maps:get(<<"requestBody">>, RawOperation, undefined),
    {RequestBodySchema, RequestBodyExtraSchemas, RequestBodyCTX} = parse_request_body(
        RawRequestBody, ParametersCTX
    ),
    RequestBodySchemas = [{RequestBodyRef, RequestBodySchema} | RequestBodyExtraSchemas],

    {Responses, ResponsesSchemas, ResponsesCTX} =
        lists:foldl(
            fun(
                {RawStatusCode, RawResponse},
                {ResponsesAcc, ResponsesExtraSchemasAcc, ResponsesCTXAcc}
            ) ->
                StatusCode =
                    case RawStatusCode of
                        <<"default">> ->
                            '*';
                        _RawStatusCode ->
                            erlang:binary_to_integer(RawStatusCode)
                    end,
                {ResponseBody, ResponseExtraSchemas, ResponseCTX} = parse_response_body(
                    RawResponse, ResponsesCTXAcc
                ),
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
                {
                    ResponsesAcc#{StatusCode => Ref},
                    [{Ref, ResponseBody} | ResponseExtraSchemas] ++ ResponsesExtraSchemasAcc,
                    ResponseCTX
                }
            end,
            {#{}, [], RequestBodyCTX},
            maps:to_list(RawResponses)
        ),

    Operation = #{
        id => OperationId,
        method => Method,
        parameters => Parameters,
        request_body => RequestBodyRef,
        responses => Responses
    },
    Schemas = ParametersSchemas ++ RequestBodySchemas ++ ResponsesSchemas,
    {Operation, Schemas, ResponsesCTX}.

-spec parse_parameter(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: ctx(),
    Result :: {Parameter, ExtraSchemas, NewCTX},
    Parameter :: erf_parser:parameter(),
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_parameter(#{<<"$ref">> := Ref}, CTX) ->
    {_RefResolved, RefOAS, RefCTX} = resolve_ref(Ref, CTX),
    parse_parameter(RefOAS, RefCTX);
parse_parameter(#{<<"content">> := Content} = RawParameter, #ctx{namespace = Namespace} = CTX) ->
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
    ParameterRef = erf_util:to_snake_case(<<Namespace/binary, "_", ParameterName/binary>>),
    Parameter = #{
        ref => ParameterRef,
        name => ParameterName,
        type => ParameterType
    },
    {AnyOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun({_MediaType, #{<<"schema">> := RawSchema}}, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, SchemaCTX} = parse_schema(RawSchema, CTXAcc),
                {
                    [Schema#{<<"nullable">> => not Required} | AnyOfAcc],
                    ExtraSchemas ++ ExtraSchemasAcc,
                    SchemaCTX
                }
            end,
            {[], [], CTX},
            maps:to_list(Content)
        ),
    ParameterSchema = #{<<"anyOf">> => AnyOf},
    {Parameter, [{ParameterRef, ParameterSchema} | ExtraSchemas], NewCTX};
parse_parameter(#{<<"schema">> := RawSchema} = RawParameter, #ctx{namespace = Namespace} = CTX) ->
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
    ParameterRef = erf_util:to_snake_case(<<Namespace/binary, "_", ParameterName/binary>>),
    Parameter = #{
        ref => ParameterRef,
        name => ParameterName,
        type => ParameterType
    },
    ParameterSchema = RawSchema#{<<"nullable">> => not Required},
    {Parameter, [{ParameterRef, ParameterSchema}], CTX}.

-spec parse_request_body(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: ctx(),
    Result :: {RequestBody, ExtraSchemas, NewCTX},
    RequestBody :: erf_parser:schema(),
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_request_body(#{<<"$ref">> := Ref}, CTX) ->
    {RefResolved, RefOAS, RefCTX} = resolve_ref(Ref, CTX),
    {NewSchema, NewExtraSchemas, NewCTX} = parse_request_body(RefOAS, RefCTX),
    {#{<<"$ref">> => RefResolved}, [{RefResolved, NewSchema} | NewExtraSchemas], NewCTX};
parse_request_body(#{<<"content">> := Content} = ReqBody, CTX) ->
    Required = maps:get(<<"required">>, ReqBody, false),
    {AnyOf, NewExtraSchemas, NewCTX} = lists:foldl(
        fun({_MediaType, #{<<"schema">> := RawSchema}}, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
            {Schema, ExtraSchemas, SchemaCTX} = parse_schema(RawSchema, CTXAcc),
            {
                [Schema#{<<"nullable">> => not Required} | AnyOfAcc],
                ExtraSchemas ++ ExtraSchemasAcc,
                SchemaCTX
            }
        end,
        {[], [], CTX},
        maps:to_list(Content)
    ),
    {#{<<"anyOf">> => AnyOf}, NewExtraSchemas, NewCTX};
parse_request_body(_ReqBody, CTX) ->
    {undefined, [], CTX}.

-spec parse_response_body(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: ctx(),
    Result :: {ResponseBody, ExtraSchemas, NewCTX},
    ResponseBody :: erf_parser:schema(),
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_response_body(#{<<"$ref">> := Ref}, CTX) ->
    {RefResolved, RefOAS, RefCTX} = resolve_ref(Ref, CTX),
    {NewSchema, NewExtraSchemas, NewCTX} = parse_response_body(RefOAS, RefCTX),
    {#{<<"$ref">> => RefResolved}, [{RefResolved, NewSchema} | NewExtraSchemas], NewCTX};
parse_response_body(#{<<"content">> := Content}, CTX) ->
    {AnyOf, NewExtraSchemas, NewCTX} = lists:foldl(
        fun({_MediaType, #{<<"schema">> := RawSchema}}, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
            {Schema, ExtraSchemas, SchemaCTX} = parse_schema(RawSchema, CTXAcc),
            {[Schema | AnyOfAcc], ExtraSchemas ++ ExtraSchemasAcc, SchemaCTX}
        end,
        {[], [], CTX},
        maps:to_list(Content)
    ),
    {#{<<"anyOf">> => AnyOf}, NewExtraSchemas, NewCTX};
parse_response_body(_Response, CTX) ->
    {undefined, [], CTX}.

-spec parse_schema(OAS, CTX) -> Result when
    OAS :: oas(),
    CTX :: ctx(),
    Result :: {Schema, ExtraSchemas, NewCTX},
    Schema :: erf_parser:schema(),
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_schema(#{<<"$ref">> := Ref}, CTX) ->
    {RefResolved, RefOAS, RefCTX} = resolve_ref(Ref, CTX),
    {NewSchema, NewExtraSchemas, NewCTX} = parse_schema(RefOAS, RefCTX),
    {#{<<"$ref">> => RefResolved}, [{RefResolved, NewSchema} | NewExtraSchemas], NewCTX};
parse_schema(#{<<"items">> := RawItems} = Schema, CTX) ->
    {Items, NewExtraSchemas, NewCTX} = parse_schema(RawItems, CTX),
    {Schema#{<<"items">> => Items}, NewExtraSchemas, NewCTX};
parse_schema(#{<<"properties">> := RawProperties} = Schema, CTX) ->
    {Properties, PropertiesExtraSchemas, PropertiesCTX} = lists:foldl(
        fun({Name, RawProperty}, {PropertiesAcc, ExtraSchemasAcc, CTXAcc}) ->
            {Property, ExtraSchemas, PropertyCTX} = parse_schema(RawProperty, CTXAcc),
            {PropertiesAcc#{Name => Property}, ExtraSchemas ++ ExtraSchemasAcc, PropertyCTX}
        end,
        {#{}, [], CTX},
        maps:to_list(RawProperties)
    ),
    PropertiesSchema = Schema#{<<"properties">> => Properties},
    RawAdditionalProperties = maps:get(<<"additionalProperties">>, Schema, undefined),
    {AdditionalProperties, AdditionalPropertiesExtraSchemas, AdditionalPropertiesCTX} = parse_schema(
        RawAdditionalProperties, PropertiesCTX
    ),
    AdditionalPropertiesSchema = PropertiesSchema#{
        <<"additionalProperties">> => AdditionalProperties
    },
    {AdditionalPropertiesSchema, PropertiesExtraSchemas ++ AdditionalPropertiesExtraSchemas,
        AdditionalPropertiesCTX};
parse_schema(#{<<"allOf">> := RawAllOf}, CTX) ->
    {AllOf, NewExtraSchemas, NewCTX} = lists:foldl(
        fun(RawSchema, {AllOfAcc, ExtraSchemasAcc, CTXAcc}) ->
            {Schema, ExtraSchemas, SchemaCTX} = parse_schema(RawSchema, CTXAcc),
            {[Schema | AllOfAcc], ExtraSchemas ++ ExtraSchemasAcc, SchemaCTX}
        end,
        {[], [], CTX},
        RawAllOf
    ),
    {#{<<"allOf">> => AllOf}, NewExtraSchemas, NewCTX};
parse_schema(#{<<"oneOf">> := RawOneOf}, CTX) ->
    {OneOf, NewExtraSchemas, NewCTX} = lists:foldl(
        fun(RawSchema, {OneOfAcc, ExtraSchemasAcc, CTXAcc}) ->
            {Schema, ExtraSchemas, SchemaCTX} = parse_schema(RawSchema, CTXAcc),
            {[Schema | OneOfAcc], ExtraSchemas ++ ExtraSchemasAcc, SchemaCTX}
        end,
        {[], [], CTX},
        RawOneOf
    ),
    {#{<<"oneOf">> => OneOf}, NewExtraSchemas, NewCTX};
parse_schema(#{<<"anyOf">> := RawAnyOf}, CTX) ->
    {AnyOf, NewExtraSchemas, NewCTX} = lists:foldl(
        fun(RawSchema, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
            {Schema, ExtraSchemas, SchemaCTX} = parse_schema(RawSchema, CTXAcc),
            {[Schema | AnyOfAcc], ExtraSchemas ++ ExtraSchemasAcc, SchemaCTX}
        end,
        {[], [], CTX},
        RawAnyOf
    ),
    {#{<<"anyOf">> => AnyOf}, NewExtraSchemas, NewCTX};
parse_schema(#{<<"not">> := RawNot}, CTX) ->
    {Not, NewExtraSchemas, NewCTX} = parse_schema(RawNot, CTX),
    {#{<<"not">> => Not}, NewExtraSchemas, NewCTX};
parse_schema(Schema, CTX) ->
    {Schema, [], CTX}.

-spec parse_version(OAS) -> Version when
    OAS :: oas(),
    Version :: binary().
parse_version(#{<<"info">> := #{<<"version">> := Version}}) ->
    Version.

-spec read_spec(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, BinSpec} | {error, Reason},
    BinSpec :: binary(),
    Reason :: term().
read_spec(SpecPath) ->
    case file:read_file(SpecPath) of
        {ok, BinSpec} ->
            {ok, BinSpec};
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

-spec resolve_ref(Ref, CTX) -> Result when
    Ref :: binary(),
    CTX :: ctx(),
    Result :: {NewResolved, NewOAS, NewCTX},
    NewResolved :: binary(),
    NewOAS :: oas(),
    NewCTX :: ctx().
resolve_ref(Ref, #ctx{base_path = BasePath, resolved = Resolved, spec = Spec}) ->
    [FilePath, ElementPath] = binary:split(Ref, <<"#">>, [global]),
    [<<>> | LocalPath] = binary:split(ElementPath, <<"/">>, [global]),
    {NewSpec, NewBasePath, NewNamespace} =
        case FilePath of
            <<>> ->
                ResetNamespace = filename:rootname(filename:basename(BasePath)),
                {Spec, BasePath, ResetNamespace};
            _FilePath ->
                RefBasePath = filename:join(filename:dirname(BasePath), FilePath),
                case read_spec(RefBasePath) of
                    {ok, Bin} ->
                        case deserialize_spec(Bin) of
                            {ok, RefSpec} ->
                                RefNamespace = filename:rootname(filename:basename(FilePath)),
                                {RefSpec, RefBasePath, RefNamespace};
                            {error, Reason} ->
                                erlang:error({invalid_ref, Reason})
                        end;
                    {error, Reason} ->
                        erlang:error({invalid_ref, Reason})
                end
        end,
    NewResolved = <<NewNamespace/binary, "_", (lists:last(LocalPath))/binary>>,
    NewOAS = get(LocalPath, NewSpec),
    NewCTX = #ctx{
        base_path = NewBasePath,
        namespace = NewNamespace,
        resolved = [NewResolved | Resolved],
        spec = NewSpec
    },
    {NewResolved, NewOAS, NewCTX}.
