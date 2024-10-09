%%% Copyright 2024 Nomasystems, S.L. http://www.nomasystems.com
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

%% @doc An <code>elli</code> implementation for <code>erf_http_server</code>.
-module(erf_http_server_elli).

%%% BEHAVIOURS
-behaviour(erf_http_server).

%%% INCLUDE FILES
-include_lib("kernel/include/logger.hrl").

%%% ERF HTTP SERVER EXPORTS
-export([
    start_link/3
]).

%%% ELLI HANDLER EXPORTS
-export([
    handle/2,
    handle_event/3
]).

%%% TYPES
-type extra_conf() :: #{
    min_acceptors => pos_integer(),
    accept_timeout => pos_integer(),
    request_timeout => pos_integer(),
    header_timeout => pos_integer(),
    body_timeout => pos_integer(),
    max_body_size => pos_integer()
}.

%%% TYPE EXPORTS
-export_type([
    extra_conf/0
]).

%%% MACROS
-define(HTTP_SERVER_NAME(Name),
    (erlang:binary_to_atom(<<"erf_http_server_", (erlang:atom_to_binary(Name))/binary>>))
).

%%%-----------------------------------------------------------------------------
%%% ERF HTTP SERVER EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(Name, Conf, ExtraConf) -> Result when
    Name :: atom(),
    Conf :: erf_http_server:conf(),
    ExtraConf :: extra_conf(),
    Result :: supervisor:startlink_ret().
start_link(Name, Conf, ExtraConf) ->
    ElliConf = build_elli_conf(Name, Conf, ExtraConf),
    elli:start_link(ElliConf).

%%%-----------------------------------------------------------------------------
%%% ELLI HANDLER EXPORTS
%%%-----------------------------------------------------------------------------
-spec handle(InitialRequest, CallbackArgs) -> Result when
    InitialRequest :: elli:req(),
    CallbackArgs :: [Name :: atom()],
    Result :: elli_handler:result().
%% @doc Handles an HTTP request.
%% @private
handle(ElliRequest, [Name]) ->
    ErfRequest = preprocess(Name, ElliRequest),
    ErfResponse = erf_router:handle(Name, ErfRequest),
    postprocess(ErfRequest, ErfResponse).

-spec handle_event(Event, Args, CallbackArgs) -> ok when
    Event :: elli_handler:event(),
    Args :: term(),
    CallbackArgs :: [Name :: atom()].
%% @doc Handles an elli event.
%% @private
handle_event(request_complete, Args, CallbackArgs) ->
    handle_full_response(request_complete, Args, CallbackArgs);
handle_event(chunk_complete, Args, CallbackArgs) ->
    handle_full_response(chunk_complete, Args, CallbackArgs);
handle_event(invalid_return, [Request, Unexpected], CallbackArgs) ->
    handle_exception(Request, Unexpected, CallbackArgs);
handle_event(request_throw, [Request, Exception, Stacktrace], [Name]) ->
    handle_exception(Request, [Exception, Stacktrace], [Name]),
    {ok, LogLevel} = erf_conf:log_level(Name),
    ?LOG(LogLevel, "[erf] Request ~p threw exception ~p:~n~p", [Request, Exception, Stacktrace]);
handle_event(request_error, [Request, Exception, Stacktrace], [Name]) ->
    handle_exception(Request, [Exception, Stacktrace], [Name]),
    {ok, LogLevel} = erf_conf:log_level(Name),
    ?LOG(LogLevel, "[erf] Request ~p errored with exception ~p.~nStacktrace:~n~p", [
        preprocess(Name, Request), Exception, Stacktrace
    ]);
handle_event(request_exit, [Request, Exception, Stacktrace], [Name]) ->
    handle_exception(Request, [Exception, Stacktrace], [Name]),
    {ok, LogLevel} = erf_conf:log_level(Name),
    ?LOG(LogLevel, "[erf] Request ~p exited with exception ~p.~nStacktrace:~n~p", [
        preprocess(Name, Request), Exception, Stacktrace
    ]);
handle_event(file_error, [ErrorReason], [Name]) ->
    {ok, LogLevel} = erf_conf:log_level(Name),
    ?LOG(LogLevel, "[erf] Returning file errored with reason: ~p", [ErrorReason]);
handle_event(_Event, _Args, _CallbackArgs) ->
    % TODO: take better advantage of the event system
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec build_elli_conf(Name, HTTPServerConf, ExtraElliConf) -> ElliConf when
    Name :: atom(),
    HTTPServerConf :: erf_http_server:conf(),
    ExtraElliConf :: erf_http_server:extra_conf(),
    ElliConf :: [{atom(), term()}].
build_elli_conf(Name, HTTPServerConf, ExtraElliConf) ->
    lists:filter(
        fun
            ({_K, undefined}) -> false;
            ({_K, _V}) -> true
        end,
        [
            {name, {local, ?HTTP_SERVER_NAME(Name)}},
            {callback, ?MODULE},
            {callback_args, [Name]},
            {port, maps:get(port, HTTPServerConf, 8080)},
            {ssl, maps:get(ssl, HTTPServerConf, false)},
            {certfile, maps:get(certfile, HTTPServerConf, undefined)},
            {keyfile, maps:get(keyfile, HTTPServerConf, undefined)},
            {min_acceptors, maps:get(min_acceptors, ExtraElliConf, 20)},
            {accept_timeout, maps:get(accept_timeout, ExtraElliConf, 10000)},
            {request_timeout, maps:get(request_timeout, ExtraElliConf, 60000)},
            {header_timeout, maps:get(header_timeout, ExtraElliConf, 10000)},
            {body_timeout, maps:get(body_timeout, ExtraElliConf, 30000)},
            {max_body_size, maps:get(max_body_size, ExtraElliConf, 1024000)}
        ]
    ).

-spec duration(Timings, Key) -> Result when
    Timings :: list(),
    Key :: atom(),
    Result :: integer().
duration(Timings, request) ->
    duration(request_start, request_end, Timings);
duration(Timings, req_body) ->
    duration(body_start, body_end, Timings);
duration(Timings, user) ->
    duration(user_start, user_end, Timings).

-spec duration(StartKey, EndKey, Timings) -> Result when
    StartKey :: atom(),
    EndKey :: atom(),
    Timings :: list(),
    Result :: integer().
duration(StartKey, EndKey, Timings) ->
    Start = proplists:get_value(StartKey, Timings),
    End   = proplists:get_value(EndKey, Timings),
    End - Start.

-spec handle_full_response(Event, Args, Config) -> ok when
    Event :: elli_handler:event(),
    Args :: elli_handler:callback_args(),
    Config :: [Name :: atom()].
handle_full_response(Event, [RawReq, StatusCode, Hs, Body, {Timings, Sizes}], [Name]) ->
    Metrics = #{
        duration => duration(Timings, request),
        req_body_duration => duration(Timings, req_body),
        req_body_length => size(Sizes, request_body), 
        resp_body_length => size(Sizes, response_body),
        resp_duration => duration(Timings, user)
    },
    Req = preprocess(Name, RawReq),
    erf_telemetry:event({Event, Metrics}, Name, Req, {StatusCode, Hs, Body}).

-spec handle_exception(RawReq, Args, Config) -> ok when
    RawReq :: elli:req(),
    Args :: term(),
    Config :: [Name :: atom()].
handle_exception(RawReq, [Exception, Stacktrace], [Name]) ->
    Req = preprocess(Name, RawReq),
    ExceptionData = #{
        stacktrace => erlang:list_to_binary(io_lib:format("~p", [Stacktrace])),
        error => erlang:list_to_binary(io_lib:format("~p", [Exception]))
    },
    erf_telemetry:event({request_exception, ExceptionData}, Name, Req, {500, [], undefined});
handle_exception(RawReq, Unexpected, [Name]) ->
    Req = preprocess(Name, RawReq),
    ExceptionData = #{
        error => erlang:list_to_binary(io_lib:format("~p", [Unexpected]))
    },
    erf_telemetry:event({request_exception, ExceptionData}, Name, Req, {500, [], undefined}).

-spec postprocess(Request, Response) -> Resp when
    Request :: erf:request(),
    Response :: erf:response(),
    Resp :: elli_handler:result().
postprocess(
    #{
        headers := ReqHeaders
    } = _Request,
    {Status, Headers, {file, File}}
) ->
    % File responses are handled by elli_sendfile
    Range = elli_request:get_range(
        % elli:req() mock
        {req, 'GET', undefined, undefined, undefined, [], [], <<>>, {1, 1}, ReqHeaders, ReqHeaders,
            <<>>, erlang:self(), undefined, {undefined, []}}
    ),
    {Status, Headers, {file, File, Range}};
postprocess(_Request, {Status, RawHeaders, RawBody}) ->
    {Status, RawHeaders, RawBody}.

-spec preprocess(Name, Req) -> Request when
    Name :: atom(),
    Req :: elli:req(),
    Request :: erf:request().
preprocess(Name, Req) ->
    Scheme = elli_request:scheme(Req),
    {Host, Port} = host_port(Req),
    Path = elli_request:path(Req),
    Method = preprocess_method(elli_request:method(Req)),
    QueryParameters = elli_request:get_args_decoded(Req),
    Headers = elli_request:headers(Req),
    Peer = elli_request:peer(Req),
    RawBody =
        case elli_request:body(Req) of
            <<>> ->
                undefined;
            ElliBody ->
                ElliBody
        end,
    JoinPath = erlang:list_to_binary([<<"/">> | lists:join(<<"/">>, Path)]),
    Route = 
        case erf:match_route(Name, JoinPath) of
            {ok, R} ->
                R;
            {error, not_found} ->
                JoinPath
        end,
    #{
        scheme => Scheme,
        host => Host,
        port => Port,
        path => Path,
        method => Method,
        query_parameters => QueryParameters,
        headers => Headers,
        body => RawBody,
        peer => Peer,
        route => Route
    }.

-spec preprocess_method(ElliMethod) -> Result when
    ElliMethod :: elli:http_method(),
    Result :: erf:method().
preprocess_method('GET') ->
    get;
preprocess_method('POST') ->
    post;
preprocess_method('PUT') ->
    put;
preprocess_method('DELETE') ->
    delete;
preprocess_method(<<"PATCH">>) ->
    patch;
preprocess_method('HEAD') ->
    head;
preprocess_method('OPTIONS') ->
    options;
preprocess_method('TRACE') ->
    trace;
preprocess_method(<<"CONNECT">>) ->
    connect.

-spec size(Sizes, Key) -> Result when
    Sizes :: list(),
    Key :: atom(),
    Result :: integer().
size(Sizes, request_body) ->
    proplists:get_value(req_body, Sizes);
size(Sizes, response_body) ->
    case proplists:get_value(chunks, Sizes) of
        undefined ->
        case proplists:get_value(file, Sizes) of
            undefined ->
                proplists:get_value(resp_body, Sizes);
            FileSize -> 
                FileSize
        end;
        ChunksSize -> ChunksSize
    end.

host_port(Req) ->
    case {elli_request:host(Req), elli_request:port(Req)} of
        {undefined, Port} ->
            HostHeader = proplists:get_value(<<"host">>, elli_request:headers(Req), <<"">>),
            case string:split(HostHeader, <<":">>) of
                [RawHost, RawPort] ->
                    {RawHost, erlang:binary_to_integer(RawPort)};
                [RawHost] ->
                    {RawHost, Port};
                _Error ->
                    {undefined, Port}
            end;
        {Host, Port} ->
            {Host, Port}
    end.
