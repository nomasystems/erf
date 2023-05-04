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
    Components = parse_components(Val, CTX#ctx{namespace = Name}),
    Endpoints = parse_endpoints(Val),
    #{
        name => Name,
        version => Version,
        endpoints => Endpoints,
        components => Components
    }.

parse_components(Val, #ctx{namespace = NS} = CTX) ->
    EndpointsComponents = lists:flatmap(
        fun(E) ->
            parse_endpoint_components(E, CTX)
        end,
        maps:to_list(maps:get(<<"paths">>, Val))
    ),
    OASComponents = maps:get(<<"components">>, Val, #{}),
    Components = lists:flatten([
        lists:map(
            fun({SchemaName, Schema}) ->
                {
                    <<NS/binary, "_", (erf_parser:to_snake_case(SchemaName))/binary>>,
                    parse_schema(Schema, CTX)
                }
            end,
            maps:to_list(maps:get(<<"schemas">>, OASComponents, #{}))
        ),
        lists:map(
            fun({ResponseName, Response}) ->
                {
                    <<NS/binary, "_", (erf_parser:to_snake_case(ResponseName))/binary>>,
                    parse_response(Response, CTX)
                }
            end,
            maps:to_list(maps:get(<<"responses">>, OASComponents, #{}))
        ),
        lists:map(
            fun({ParameterName, Parameter}) ->
                {
                    <<NS/binary, "_", (erf_parser:to_snake_case(ParameterName))/binary>>,
                    parse_parameter(Parameter, CTX)
                }
            end,
            maps:to_list(maps:get(<<"parameters">>, OASComponents, #{}))
        ),
        lists:map(
            fun({RequestBodyName, RequestBody}) ->
                {
                    <<NS/binary, "_", (erf_parser:to_snake_case(RequestBodyName))/binary>>,
                    parse_request_body(RequestBody, CTX)
                }
            end,
            maps:to_list(maps:get(<<"requestBodies">>, OASComponents, #{}))
        )
    ]),
    lists:delete(
        undefined,
        lists:uniq(EndpointsComponents ++ Components)
    ).

parse_endpoint_components({Path, Endpoint}, #ctx{namespace = NS} = CTX) ->
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
    OperationsComponents = lists:map(
        fun(RawOperation) ->
            parse_operation_components(RawOperation, CTX)
        end,
        Operations
    ),
    lists:flatten([EndpointParameters | OperationsComponents]).

parse_endpoints(_OAS) ->
    %% TODO: Implement
    [].

parse_name(#{<<"info">> := #{<<"title">> := Name}}) ->
    erf_parser:to_snake_case(Name).

parse_operation_components(
    {Path, Method, #{<<"responses">> := Responses} = Operation}, #ctx{namespace = NS} = CTX
) ->
    NewCTX =
        case maps:get(<<"operationId">>, Operation, undefined) of
            undefined ->
                CTX#ctx{
                    namespace = erf_parser:to_snake_case(
                        <<NS/binary, Path/binary, "_", Method/binary>>
                    )
                };
            OperationId ->
                CTX#ctx{
                    namespace = erf_parser:to_snake_case(<<NS/binary, "_", OperationId/binary>>)
                }
        end,
    Parameters =
        lists:map(
            fun(RawParameter) ->
                parse_parameter(RawParameter, NewCTX)
            end,
            maps:get(<<"parameters">>, Operation, [])
        ),
    RawRequestBody = maps:get(<<"requestBody">>, Operation, undefined),
    RequestBody = parse_request_body(RawRequestBody, NewCTX),
    ResponsesComponents = lists:map(
        fun({_StatusCode, RawResponse}) ->
            parse_response(RawResponse, NewCTX)
        end,
        maps:to_list(Responses)
    ),
    lists:flatten([Parameters, RequestBody, ResponsesComponents]).

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
    {<<NS/binary, "_", Name/binary>>, #{
        type => Type,
        schema => #{
            <<"anyOf">> =>
                lists:map(
                    fun({_MediaType, #{<<"schema">> := RawSchema}}) ->
                        Schema = parse_schema(RawSchema, CTX),
                        Schema#{<<"nullable">> => not Required}
                    end,
                    maps:to_list(Content)
                )
        }
    }};
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
    {<<NS/binary, "_", Name/binary>>, #{
        type => Type,
        schema => Schema#{<<"nullable">> => not Required}
    }}.

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
    {<<NS/binary, "_request_body">>, #{
        type => request_body,
        schema => #{
            <<"anyOf">> =>
                lists:map(
                    fun({_MediaType, #{<<"schema">> := RawSchema}}) ->
                        Schema = parse_schema(RawSchema, CTX),
                        Schema#{<<"nullable">> => not Required}
                    end,
                    maps:to_list(Content)
                )
        }
    }};
parse_request_body(_RequestBody, _CTX) ->
    undefined.

parse_response(#{<<"$ref">> := Ref}, CTX) ->
    Response = parse_ref(Ref, CTX),
    parse_response(Response, CTX);
parse_response(#{<<"content">> := Content}, #ctx{namespace = NS} = CTX) ->
    {<<NS/binary, "_response_body">>, #{
        type => response_body,
        schema => #{
            <<"anyOf">> =>
                lists:map(
                    fun({_MediaType, #{<<"schema">> := Schema}}) ->
                        parse_schema(Schema, CTX)
                    end,
                    maps:to_list(Content)
                )
        }
    }};
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
