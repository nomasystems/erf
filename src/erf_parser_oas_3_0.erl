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
    oas :: map()
}).

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
            Val = njson:decode(Bin),
            case oas_3_0:is_valid(Val) of
                true ->
                    BasePath = filename:dirname(SpecPath),
                    {ok, parse_api(Val, #ctx{base_path = BasePath, oas = Val})};
                false ->
                    {error, {invalid_spec, <<"Invalid OpenAPI Specification 3.0">>}}
            end;
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
get([], Val) ->
    Val;
get([Key | Keys], Val) ->
    get(Keys, maps:get(Key, Val)).

parse_api(Val, CTX) ->
    Name = parse_name(Val),
    Version = parse_version(Val),
    Schemas = parse_schemas(Val, CTX),
    Endpoints = parse_endpoints(Val),
    #{
        name => Name,
        version => Version,
        endpoints => Endpoints,
        schemas => Schemas
    }.

parse_schemas(Val, CTX) ->
    maps:from_list(
        lists:flatmap(
            fun(E) ->
                parse_endpoint_schemas(E, CTX)
            end,
            maps:to_list(maps:get(<<"paths">>, Val))
        )
    ).

parse_endpoint_schemas({Path, Endpoint}, #ctx{namespace = NS} = CTX) ->
    NewCTX = CTX#ctx{namespace = erf_parser:to_snake_case(<<NS/binary, Path/binary>>)},
    EndpointParameters =
        lists:map(
            fun(RawParameter) ->
                parse_parameter(RawParameter, NewCTX)
            end,
            maps:get(<<"parameters">>, Endpoint, [])
        ),
    Operations =
        lists:filtermap(
            fun(Method) ->
                case maps:get(Method, Endpoint, undefined) of
                    undefined ->
                        false;
                    Operation ->
                        {true, {Path, Method, Operation}}
                end
            end,
            ?METHODS
        ),
    OperationsSchemas = lists:flatmap(
        fun(RawOperation) ->
            parse_operation_schemas(RawOperation, CTX)
        end,
        Operations
    ),
    EndpointParameters ++ OperationsSchemas.

parse_endpoints(_OAS) ->
    %% TODO: Implement
    [].

parse_name(#{<<"info">> := #{<<"title">> := Name}}) ->
    erf_parser:to_snake_case(Name).

parse_operation_schemas(
    {Path, Method, #{<<"responses">> := Responses} = Operation}, CTX
) ->
    NewCTX =
        case maps:get(<<"operationId">>, Operation, undefined) of
            undefined ->
                CTX#ctx{namespace = erf_parser:to_snake_case(<<Path/binary, "_", Method/binary>>)};
            OperationId ->
                CTX#ctx{namespace = erf_parser:to_snake_case(OperationId)}
        end,
    Parameters =
        lists:map(
            fun(RawParameter) ->
                parse_parameter(RawParameter, NewCTX)
            end,
            maps:get(<<"parameters">>, Operation, [])
        ),
    RequestBodyName = erf_parser:to_snake_case(<<(NewCTX#ctx.namespace)/binary, "_request_body">>),
    RequestBodySchema =
        case maps:get(<<"requestBody">>, Operation, undefined) of
            undefined ->
                undefined;
            RawRequestBody ->
                parse_request_body(RawRequestBody, NewCTX)
        end,
    RequestBody = {RequestBodyName, RequestBodySchema},
    ResponseBodyName = erf_parser:to_snake_case(<<(NewCTX#ctx.namespace)/binary, "_response_body">>),
    ResponseBodySchema = #{
        <<"anyOf">> =>
            lists:map(
                fun({_StatusCode, RawResponse}) ->
                    parse_response(RawResponse, NewCTX)
                end,
                maps:to_list(Responses)
            )
    },
    ResponseBody = {ResponseBodyName, ResponseBodySchema},
    lists:flatten([RequestBody, ResponseBody | Parameters]).

parse_parameter(#{<<"$ref">> := Ref}, CTX) ->
    Parameter = parse_ref(Ref, CTX),
    parse_parameter(Parameter, CTX);
parse_parameter(#{<<"content">> := Content} = Parameter, #ctx{namespace = NS} = CTX) ->
    Name = maps:get(<<"name">>, Parameter),
    Type =
        case maps:get(<<"in">>, Parameter) of
            <<"query">> ->
                query_parameter;
            <<"header">> ->
                header;
            <<"path">> ->
                path_parameter;
            <<"cookie">> ->
                cookie
        end,
    DefaultRequired =
        case Type of
            path_parameter ->
                true;
            _parameter ->
                false
        end,
    Required = maps:get(<<"required">>, Parameter, DefaultRequired),
    ParameterName = erf_parser:to_snake_case(<<NS/binary, "_", Name/binary>>),
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
    {ParameterName, ParameterSchema};
parse_parameter(#{<<"schema">> := RawSchema} = Parameter, #ctx{namespace = NS} = CTX) ->
    Name = maps:get(<<"name">>, Parameter),
    Type =
        case maps:get(<<"in">>, Parameter) of
            <<"query">> ->
                query_parameter;
            <<"header">> ->
                header;
            <<"path">> ->
                path_parameter;
            <<"cookie">> ->
                cookie
        end,
    DefaultRequired =
        case Type of
            path_parameter ->
                true;
            _parameter ->
                false
        end,
    Required = maps:get(<<"required">>, Parameter, DefaultRequired),
    Schema = parse_schema(RawSchema, CTX),
    ParameterName = erf_parser:to_snake_case(<<NS/binary, "_", Name/binary>>),
    ParameterSchema = Schema#{<<"nullable">> => not Required},
    {ParameterName, ParameterSchema}.

parse_ref(Ref, #ctx{base_path = BasePath, oas = OAS}) ->
    [FilePath, ElementPath] = binary:split(Ref, <<"#">>, [global]),
    Schema =
        case FilePath of
            <<>> ->
                OAS;
            _FilePath ->
                AbsPath = filename:join(BasePath, FilePath),
                case file:read_file(AbsPath) of
                    {ok, Bin} ->
                        njson:decode(Bin);
                    {error, Reason} ->
                        erlang:error({invalid_spec, Reason})
                end
        end,
    [<<>> | Steps] = binary:split(ElementPath, <<"/">>, [global]),
    get(Steps, Schema).

parse_request_body(#{<<"$ref">> := Ref}, CTX) ->
    RequestBody = parse_ref(Ref, CTX),
    parse_request_body(RequestBody, CTX);
parse_request_body(#{<<"content">> := Content} = ReqBody, #ctx{namespace = NS} = CTX) ->
    Required = maps:get(<<"required">>, ReqBody, false),
    RequestBodyName = erf_parser:to_snake_case(<<NS/binary, "_request_body">>),
    RequestBodySchema = #{
        <<"anyOf">> =>
            lists:map(
                fun({_MediaType, #{<<"schema">> := RawSchema}}) ->
                    Schema = parse_schema(RawSchema, CTX),
                    Schema#{<<"nullable">> => not Required}
                end,
                maps:to_list(Content)
            )
    },
    {RequestBodyName, RequestBodySchema}.

parse_response(#{<<"$ref">> := Ref}, CTX) ->
    Response = parse_ref(Ref, CTX),
    parse_response(Response, CTX);
parse_response(#{<<"content">> := Content}, CTX) ->
    #{
        <<"anyOf">> =>
            lists:map(
                fun({_MediaType, #{<<"schema">> := Schema}}) ->
                    parse_schema(Schema, CTX)
                end,
                maps:to_list(Content)
            )
    };
parse_response(_Response, _CTX) ->
    undefined.

parse_schema(#{<<"$ref">> := Ref}, CTX) ->
    Schema = parse_ref(Ref, CTX),
    parse_schema(Schema, CTX);
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

parse_version(#{<<"info">> := #{<<"version">> := Version}}) ->
    Version.
