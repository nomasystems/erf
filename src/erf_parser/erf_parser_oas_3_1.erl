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

%% @doc An OpenAPI Specification 3.1.1 <code>erf_parser</code>.
-module(erf_parser_oas_3_1).

%%% BEHAVIOURS
-behaviour(erf_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/1
]).

%%% TYPES
-type ctx() :: #{
    base_path := binary(),
    base_name := binary(),
    namespace := binary(),
    resolved := [binary()],
    spec := spec()
}.
-type spec() :: njson:t().

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
    case parse_spec(SpecPath) of
        {ok, OAS} ->
            case oas_3_1:is_valid(OAS) of
                true ->
                    BasePath = filename:dirname(SpecPath),
                    BaseName = filename:rootname(filename:basename(SpecPath)),
                    CTX = #{
                        base_path => BasePath,
                        base_name => BaseName,
                        namespace => BaseName,
                        resolved => [],
                        spec => OAS
                    },
                    API = parse_api(OAS, CTX),
                    {ok, ndto_parser_json_schema:clean_optionals(API)};
                {false, Reason} ->
                    {error, {invalid_oas_3_1_spec, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_oas_3_1_spec, Reason}}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec parse_api(OAS, CTX) -> Result when
    OAS :: spec(),
    CTX :: ctx(),
    Result :: erf:api().
parse_api(OAS, CTX) ->
    Name = parse_name(OAS),
    Version = parse_version(OAS),
    {RawEndpoints, RawSchemas, _NewCTX} = lists:foldl(
        fun({Path, RawEndpoint}, {EndpointsAcc, SchemasAcc, CTXAcc}) ->
            {Endpoint, EndpointSchemas, NewCTX} = parse_endpoint(Path, RawEndpoint, CTXAcc),
            {
                [Endpoint | EndpointsAcc],
                SchemasAcc ++ EndpointSchemas,
                CTXAcc#{resolved => maps:get(resolved, NewCTX)}
            }
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
    RawEndpoint :: spec(),
    CTX :: ctx(),
    Result :: {Endpoint, Schemas, NewCTX},
    Endpoint :: erf_parser:endpoint(),
    Schemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_endpoint(Path, RawEndpoint, CTX) ->
    {Parameters, ParametersSchemas, ParametersCTX} =
        lists:foldl(
            fun(RawParameter, {ParametersAcc, ParametersExtraSchemasAcc, ParametersCTXAcc}) ->
                {Parameter, ParameterExtraSchemas, ParameterCTX} = parse_parameter(
                    RawParameter, ParametersCTXAcc
                ),
                {
                    [Parameter | ParametersAcc],
                    ParameterExtraSchemas ++ ParametersExtraSchemasAcc,
                    ParametersCTXAcc#{resolved => maps:get(resolved, ParameterCTX)}
                }
            end,
            {[], [], CTX},
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
                {
                    [Operation | OperationsAcc],
                    Schemas ++ OperationsExtraSchemasAcc,
                    OperationsCTXAcc#{resolved => maps:get(resolved, OperationCTX)}
                }
            end,
            {[], [], ParametersCTX},
            RawOperations
        ),

    Endpoint = #{
        path => Path,
        parameters => Parameters,
        operations => Operations
    },
    Schemas = ParametersSchemas ++ OperationsSchemas,
    {Endpoint, Schemas, OperationsCTX}.

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
    Val :: spec(),
    Result :: binary().
parse_name(#{<<"info">> := #{<<"title">> := Name}}) ->
    Name.

-spec parse_operation(Path, Method, RawOperation, CTX) -> Result when
    Path :: binary(),
    Method :: binary(),
    RawOperation :: spec(),
    CTX :: ctx(),
    Result :: {Operation, Schemas, NewCTX},
    Operation :: erf_parser:operation(),
    Schemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_operation(
    Path,
    RawMethod,
    #{<<"responses">> := RawResponses} = RawOperation,
    #{namespace := Namespace} = CTX
) ->
    OperationId =
        case maps:get(<<"operationId">>, RawOperation, undefined) of
            undefined ->
                erf_util:to_snake_case(<<Path/binary, "_", RawMethod/binary>>);
            RawOperationId ->
                erf_util:to_snake_case(RawOperationId)
        end,
    NewCTX = CTX#{namespace => <<Namespace/binary, "_", OperationId/binary>>},
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
                    ParametersCTXAcc#{resolved => maps:get(resolved, ParameterCTX)}
                }
            end,
            {[], [], NewCTX},
            maps:get(<<"parameters">>, RawOperation, [])
        ),

    RawRequestBody = maps:get(<<"requestBody">>, RawOperation, undefined),
    {ParsedRequestBody, RawRequestBodySchemas, RequestBodyCTX} = parse_request_body(
        RawRequestBody, ParametersCTX
    ),
    RequestBodyRef =
        erf_util:to_snake_case(<<
            (maps:get(namespace, NewCTX))/binary, "_request_body"
        >>),
    RequestBodyRequired = maps:get(required, ParsedRequestBody),
    RequestBodySchema = maps:get(schema, ParsedRequestBody),
    RequestBodySchemas = [{RequestBodyRef, RequestBodySchema} | RawRequestBodySchemas],
    RequestBody = #{
        ref => RequestBodyRef,
        required => RequestBodyRequired
    },
    Request = #{
        body => RequestBody
    },

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
                {ParsedResponse, RawResponseExtraSchemas, ResponseCTX} = parse_response(
                    RawResponse, ResponsesCTXAcc
                ),
                ResponseRef =
                    erf_util:to_snake_case(<<
                        (maps:get(namespace, NewCTX))/binary,
                        "_response_body_",
                        RawStatusCode/binary
                    >>),
                ResponseRequired = maps:get(required, ParsedResponse),
                ResponseSchema = maps:get(schema, ParsedResponse),
                ResponseExtraSchemas = [{ResponseRef, ResponseSchema} | RawResponseExtraSchemas],
                ResponseBody = #{
                    ref => ResponseRef,
                    required => ResponseRequired
                },
                Response = #{
                    body => ResponseBody
                },
                {
                    ResponsesAcc#{StatusCode => Response},
                    ResponseExtraSchemas ++ ResponsesExtraSchemasAcc,
                    ResponsesCTXAcc#{resolved => maps:get(resolved, ResponseCTX)}
                }
            end,
            {#{}, [], RequestBodyCTX},
            maps:to_list(RawResponses)
        ),

    Operation = #{
        id => OperationId,
        method => Method,
        parameters => Parameters,
        request => Request,
        responses => Responses
    },
    Schemas = ParametersSchemas ++ RequestBodySchemas ++ ResponsesSchemas,
    {Operation, Schemas, ResponsesCTX}.

-spec parse_parameter(OAS, CTX) -> Result when
    OAS :: spec(),
    CTX :: ctx(),
    Result :: {Parameter, ExtraSchemas, NewCTX},
    Parameter :: erf_parser:parameter(),
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_parameter(#{<<"$ref">> := Ref}, CTX) ->
    {_RefResolved, RefOAS, RefCTX} = resolve_ref(Ref, CTX),
    parse_parameter(RefOAS, RefCTX);
parse_parameter(#{<<"content">> := Content} = RawParameter, #{namespace := Namespace} = CTX) ->
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
        type => ParameterType,
        required => Required
    },
    {AnyOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun({_MediaType, #{<<"schema">> := RawSchema}}, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, NewExtraSchemas, NewCTX} = parse_schemas(RawSchema, CTXAcc),
                {
                    [Schema | AnyOfAcc],
                    NewExtraSchemas ++ ExtraSchemasAcc,
                    CTXAcc#{resolved => maps:get(resolved, NewCTX)}
                }
            end,
            {[], [], CTX},
            maps:to_list(Content)
        ),
    ParameterSchema = #{any_of => AnyOf},
    {Parameter, [{ParameterRef, ParameterSchema} | ExtraSchemas], NewCTX};
parse_parameter(#{<<"schema">> := RawSchema} = RawParameter, #{namespace := Namespace} = CTX) ->
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
        type => ParameterType,
        required => Required
    },
    {ParameterSchema, NewExtraSchemas, NewCTX} = parse_schemas(RawSchema, CTX),
    {Parameter, [{ParameterRef, ParameterSchema} | NewExtraSchemas], NewCTX}.

-spec parse_request_body(OAS, CTX) -> Result when
    OAS :: spec(),
    CTX :: ctx(),
    Result :: {RequestBody, ExtraSchemas, NewCTX},
    RequestBody :: #{schema := erf_parser:schema(), required := boolean()},
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_request_body(#{<<"$ref">> := Ref}, CTX) ->
    {_RefResolved, RefOAS, RefCTX} = resolve_ref(Ref, CTX),
    parse_request_body(RefOAS, RefCTX);
parse_request_body(#{<<"content">> := Content} = ReqBody, CTX) ->
    Required = maps:get(<<"required">>, ReqBody, false),
    {AnyOf, ExtraSchemas, NewCTX} = lists:foldl(
        fun({_MediaType, #{<<"schema">> := RawSchema}}, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
            {Schema, NewExtraSchemas, SchemaCTX} = parse_schemas(RawSchema, CTXAcc),
            {
                [Schema | AnyOfAcc],
                NewExtraSchemas ++ ExtraSchemasAcc,
                CTXAcc#{
                    resolved => maps:get(resolved, SchemaCTX)
                }
            }
        end,
        {[], [], CTX},
        maps:to_list(Content)
    ),
    RequestBodySchema = #{any_of => AnyOf},
    RequestBody = #{
        schema => RequestBodySchema,
        required => Required
    },
    {RequestBody, ExtraSchemas, NewCTX};
parse_request_body(_ReqBody, CTX) ->
    RequestBody = #{
        schema => true,
        required => false
    },
    {RequestBody, [], CTX}.

-spec parse_response(OAS, CTX) -> Result when
    OAS :: spec(),
    CTX :: ctx(),
    Result :: {Response, ExtraSchemas, NewCTX},
    Response :: #{schema := erf_parser:schema(), required := boolean()},
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_response(#{<<"$ref">> := Ref}, CTX) ->
    {_RefResolved, RefOAS, RefCTX} = resolve_ref(Ref, CTX),
    parse_response(RefOAS, RefCTX);
parse_response(#{<<"content">> := Content}, CTX) ->
    {AnyOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun({_MediaType, #{<<"schema">> := RawSchema}}, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, SchemaCTX} = parse_schemas(RawSchema, CTXAcc),
                {
                    [Schema | AnyOfAcc],
                    ExtraSchemas ++ ExtraSchemasAcc,
                    CTXAcc#{
                        resolved => maps:get(resolved, SchemaCTX)
                    }
                }
            end,
            {[], [], CTX},
            maps:to_list(Content)
        ),
    ResponseSchema = #{any_of => AnyOf},
    Response = #{
        schema => ResponseSchema,
        required => false
    },
    {Response, ExtraSchemas, NewCTX};
parse_response(_Response, CTX) ->
    Response = #{
        schema => true,
        required => false
    },
    {Response, [], CTX}.

-spec parse_spec(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, Spec} | {error, Reason},
    Spec :: spec(),
    Reason :: term().
parse_spec(SpecPath) ->
    case file:read_file(SpecPath) of
        {ok, BinSpec} ->
            case filename:extension(SpecPath) of
                JSON when JSON =:= <<".json">> orelse JSON =:= ".json" ->
                    case njson:decode(BinSpec) of
                        {ok, undefined} ->
                            {error, {invalid_spec, BinSpec}};
                        {ok, Spec} ->
                            {ok, Spec};
                        {error, Reason} ->
                            {error, {invalid_json, Reason}}
                    end;
                Extension ->
                    {error, {unsupported_extension, Extension}}
            end;
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

-spec parse_version(OAS) -> Version when
    OAS :: spec(),
    Version :: binary().
parse_version(#{<<"info">> := #{<<"version">> := Version}}) ->
    Version.

-spec parse_schemas(Spec, CTX) -> Result when
    Spec :: spec(),
    CTX :: ctx(),
    Result :: {Schema, ExtraSchemas, NewCTX},
    Schema :: erf_parser:schema(),
    ExtraSchemas :: [{erf_parser:ref(), erf_parser:schema()}],
    NewCTX :: ctx().
parse_schemas(false, CTX) ->
    Schema = false,
    {Schema, [], CTX};
parse_schemas(true, CTX) ->
    Schema = #{},
    {Schema, [], CTX};
parse_schemas(#{<<"$ref">> := Ref} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    {RefName, RefSchema, RefCTX} = resolve_ref(Ref, CTX),
    Schema = #{
        ref => RefName,
        nullable => Nullable
    },
    case lists:member(RefName, maps:get(resolved, CTX)) of
        true ->
            {Schema, [], CTX};
        false ->
            {NewSchema, NewExtraSchemas, NewCTX} = parse_schemas(RefSchema, RefCTX),
            {Schema, [{RefName, NewSchema} | NewExtraSchemas], NewCTX}
    end;
parse_schemas(#{<<"enum">> := Enum} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    Schema = #{
        enum => Enum,
        nullable => Nullable
    },
    {Schema, [], CTX};
parse_schemas(#{<<"type">> := <<"boolean">>} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    Schema = #{
        type => boolean,
        nullable => Nullable
    },
    {Schema, [], CTX};
parse_schemas(#{<<"type">> := <<"integer">>} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    Minimum = maps:get(<<"minimum">>, RawSchema, undefined),
    ExclusiveMinimum = maps:get(<<"exclusiveMinimum">>, RawSchema, undefined),
    Maximum = maps:get(<<"maximum">>, RawSchema, undefined),
    ExclusiveMaximum = maps:get(<<"exclusiveMaximum">>, RawSchema, undefined),
    MultipleOf = maps:get(<<"multipleOf">>, RawSchema, undefined),
    Schema =
        #{
            type => integer,
            minimum => Minimum,
            exclusive_minimum => ExclusiveMinimum,
            maximum => Maximum,
            exclusive_maximum => ExclusiveMaximum,
            multiple_of => MultipleOf,
            nullable => Nullable
        },
    {Schema, [], CTX};
parse_schemas(#{<<"type">> := <<"number">>} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    Minimum = maps:get(<<"minimum">>, RawSchema, undefined),
    ExclusiveMinimum = maps:get(<<"exclusiveMinimum">>, RawSchema, undefined),
    Maximum = maps:get(<<"maximum">>, RawSchema, undefined),
    ExclusiveMaximum = maps:get(<<"exclusiveMaximum">>, RawSchema, undefined),
    MultipleOf = maps:get(<<"multipleOf">>, RawSchema, undefined),
    Schema =
        #{
            any_of => [
                #{
                    type => integer,
                    minimum => Minimum,
                    exclusive_minimum => ExclusiveMinimum,
                    maximum => Maximum,
                    exclusive_maximum => ExclusiveMaximum,
                    multiple_of => MultipleOf
                },
                #{
                    type => float,
                    minimum => Minimum,
                    exclusive_minimum => ExclusiveMinimum,
                    maximum => Maximum,
                    exclusive_maximum => ExclusiveMaximum
                }
            ],
            nullable => Nullable
        },
    {Schema, [], CTX};
parse_schemas(#{<<"type">> := <<"string">>} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    MinLength = maps:get(<<"minLength">>, RawSchema, undefined),
    MaxLength = maps:get(<<"maxLength">>, RawSchema, undefined),
    Format =
        case maps:get(<<"format">>, RawSchema, undefined) of
            <<"iso8601">> ->
                iso8601;
            <<"byte">> ->
                base64;
            _Otherwise ->
                undefined
        end,
    Pattern = maps:get(<<"pattern">>, RawSchema, undefined),
    Schema =
        #{
            type => string,
            min_length => MinLength,
            max_length => MaxLength,
            format => Format,
            pattern => Pattern,
            nullable => Nullable
        },
    {Schema, [], CTX};
parse_schemas(#{<<"type">> := <<"array">>} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    {Items, ItemsExtraSchemas, ItemsCTX} =
        case maps:get(<<"items">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], CTX};
            RawItems ->
                parse_schemas(RawItems, CTX)
        end,
    MinItems = maps:get(<<"minItems">>, RawSchema, undefined),
    MaxItems = maps:get(<<"maxItems">>, RawSchema, undefined),
    UniqueItems = maps:get(<<"uniqueItems">>, RawSchema, undefined),
    Schema =
        #{
            type => array,
            items => Items,
            min_items => MinItems,
            max_items => MaxItems,
            unique_items => UniqueItems,
            nullable => Nullable
        },
    {Schema, ItemsExtraSchemas, ItemsCTX};
parse_schemas(#{<<"type">> := <<"object">>} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    {Properties, PropertiesExtraSchemas, PropertiesCTX} =
        case maps:get(<<"properties">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], CTX};
            RawProperties ->
                lists:foldl(
                    fun({Property, RawPropertySchema}, {PropertiesAcc, ExtraSchemasAcc, CTXAcc}) ->
                        {PropertySchema, ExtraSchemas, NewCTX} = parse_schemas(
                            RawPropertySchema, CTXAcc
                        ),
                        {
                            PropertiesAcc#{Property => PropertySchema},
                            ExtraSchemasAcc ++ ExtraSchemas,
                            CTXAcc#{resolved => maps:get(resolved, NewCTX)}
                        }
                    end,
                    {#{}, [], CTX},
                    maps:to_list(RawProperties)
                )
        end,
    Required = maps:get(<<"required">>, RawSchema, undefined),
    MinProperties = maps:get(<<"minProperties">>, RawSchema, undefined),
    MaxProperties = maps:get(<<"maxProperties">>, RawSchema, undefined),
    {AdditionalProperties, AdditionalPropertiesExtraSchemas, AdditionalPropertiesCTX} =
        case maps:get(<<"additionalProperties">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], PropertiesCTX};
            RawAdditionalProperties ->
                parse_schemas(RawAdditionalProperties, PropertiesCTX)
        end,
    Schema =
        #{
            type => object,
            properties => Properties,
            required => Required,
            min_properties => MinProperties,
            max_properties => MaxProperties,
            additional_properties => AdditionalProperties,
            nullable => Nullable
        },
    ExtraSchemas = PropertiesExtraSchemas ++ AdditionalPropertiesExtraSchemas,
    {Schema, ExtraSchemas, AdditionalPropertiesCTX};
parse_schemas(#{<<"anyOf">> := RawAnyOf} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    {AnyOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSubschema, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Subschema, ExtraSchemas, NewCTX} = parse_schemas(RawSubschema, CTXAcc),
                {
                    [Subschema | AnyOfAcc],
                    ExtraSchemasAcc ++ ExtraSchemas,
                    CTXAcc#{
                        resolved => maps:get(resolved, NewCTX)
                    }
                }
            end,
            {[], [], CTX},
            RawAnyOf
        ),
    Schema = #{
        any_of => AnyOf,
        nullable => Nullable
    },
    {Schema, ExtraSchemas, NewCTX};
parse_schemas(#{<<"allOf">> := RawAllOf} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    {AllOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSubschema, {AllOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Subschema, ExtraSchemas, NewCTX} = parse_schemas(RawSubschema, CTXAcc),
                {
                    [Subschema | AllOfAcc],
                    ExtraSchemasAcc ++ ExtraSchemas,
                    CTXAcc#{
                        resolved => maps:get(resolved, NewCTX)
                    }
                }
            end,
            {[], [], CTX},
            RawAllOf
        ),
    Schema = #{
        all_of => AllOf,
        nullable => Nullable
    },
    {Schema, ExtraSchemas, NewCTX};
parse_schemas(#{<<"not">> := RawNot} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    {Not, ExtraSchemas, NewCTX} = parse_schemas(RawNot, CTX),
    Schema = #{
        'not' => Not,
        nullable => Nullable
    },
    {Schema, ExtraSchemas, NewCTX};
parse_schemas(#{<<"oneOf">> := RawOneOf} = RawSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawSchema, undefined),
    {OneOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSubschema, {OneOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Subschema, ExtraSchemas, NewCTX} = parse_schemas(RawSubschema, CTXAcc),
                {
                    [Subschema | OneOfAcc],
                    ExtraSchemasAcc ++ ExtraSchemas,
                    CTXAcc#{
                        resolved => maps:get(resolved, NewCTX)
                    }
                }
            end,
            {[], [], CTX},
            RawOneOf
        ),
    Schema = #{
        one_of => OneOf,
        nullable => Nullable
    },
    {Schema, ExtraSchemas, NewCTX};
parse_schemas(RawUniversalSchema, CTX) ->
    Nullable = maps:get(<<"nullable">>, RawUniversalSchema, undefined),
    Schema = #{
        nullable => Nullable
    },
    {Schema, [], CTX}.

-spec resolve_ref(Ref, CTX) -> Result when
    Ref :: binary(),
    CTX :: ctx(),
    Result :: {NewResolved, NewSchema, NewCTX},
    NewResolved :: binary(),
    NewSchema :: ndto:schema(),
    NewCTX :: ctx().
resolve_ref(Ref, CTX) ->
    BasePath = maps:get(base_path, CTX),
    BaseName = maps:get(base_name, CTX),
    Resolved = maps:get(resolved, CTX),
    Spec = maps:get(spec, CTX),

    [FilePath, ElementPath] = binary:split(Ref, <<"#">>, [global]),
    LocalPath = binary:split(ElementPath, <<"/">>, [global, trim_all]),
    {NewSpec, NewBasePath, NewBaseName} =
        case FilePath of
            <<>> ->
                {Spec, BasePath, BaseName};
            _FilePath ->
                AbsPath = filename:join(BasePath, FilePath),
                case parse_spec(AbsPath) of
                    {ok, RefSpec} ->
                        RefBasePath = filename:dirname(AbsPath),
                        RefBaseName = filename:rootname(filename:basename(AbsPath)),
                        {RefSpec, RefBasePath, RefBaseName};
                    {error, Reason} ->
                        % TODO: handle error
                        erlang:error({invalid_ref, Reason})
                end
        end,
    NewResolved =
        case LocalPath of
            [] ->
                NewBaseName;
            _LocalPath ->
                erf_util:to_snake_case(<<NewBaseName/binary, "_", (lists:last(LocalPath))/binary>>)
        end,
    NewSchema = ndto_parser_json_schema:get(LocalPath, NewSpec),
    NewCTX = #{
        base_path => NewBasePath,
        base_name => NewBaseName,
        namespace => NewBaseName,
        resolved => [NewResolved | Resolved],
        spec => NewSpec
    },
    {NewResolved, NewSchema, NewCTX}.
