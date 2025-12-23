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

%% @doc A <code>cowboy</code> implementation for <code>erf_http_server</code>.
-module(erf_http_server_cowboy).

%%% BEHAVIOURS
-behaviour(erf_http_server).
%% -behaviour(cowboy_handler).
-behaviour(gen_server).

%%% INCLUDE FILES
-include_lib("kernel/include/logger.hrl").

%%% ERF HTTP SERVER EXPORTS
-export([
    start_link/3
]).

%%% COWBOY HANDLER EXPORTS
-export([
    init/2
]).

%%% GEN_SERVER EXPORTS
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%% TYPES
-type extra_conf() :: #{
    num_acceptors => pos_integer(),
    max_connections => pos_integer() | infinity
}.

-record(state, {
    listener_name :: atom()
}).

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
    gen_server:start_link(?MODULE, {Name, Conf, ExtraConf}, []).

%%%-----------------------------------------------------------------------------
%%% GEN_SERVER EXPORTS
%%%-----------------------------------------------------------------------------
init({Name, Conf, ExtraConf}) ->
    process_flag(trap_exit, true),
    ListenerName = ?HTTP_SERVER_NAME(Name),
    Port = maps:get(port, Conf, 8080),
    SSL = maps:get(ssl, Conf, false),
    NumAcceptors = maps:get(num_acceptors, ExtraConf, 100),
    MaxConnections = maps:get(max_connections, ExtraConf, infinity),
    TransportOpts = transport_opts(Port, Conf, SSL, NumAcceptors, MaxConnections),
    Dispatch = cowboy_router:compile([
        {'_', [{'_', ?MODULE, [Name]}]}
    ]),
    ProtocolOpts = #{
        env => #{dispatch => Dispatch}
    },
    Result =
        case SSL of
            true ->
                cowboy:start_tls(ListenerName, TransportOpts, ProtocolOpts);
            false ->
                cowboy:start_clear(ListenerName, TransportOpts, ProtocolOpts)
        end,
    case Result of
        {ok, _ListenerPid} ->
            {ok, #state{listener_name = ListenerName}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener_name = ListenerName}) ->
    cowboy:stop_listener(ListenerName),
    ok.

%%%-----------------------------------------------------------------------------
%%% COWBOY HANDLER EXPORTS
%%%-----------------------------------------------------------------------------
-spec init(Req, State) -> Result when
    Req :: cowboy_req:req(),
    State :: [Name :: atom()],
    Result :: {ok, cowboy_req:req(), State}.
init(CowboyReq0, [Name] = State) ->
    StartTime = erlang:monotonic_time(),
    {CowboyReq1, ErfRequest} = preprocess(Name, CowboyReq0),
    erf_telemetry:event(
        {request_start, #{monotonic_time => StartTime}},
        Name,
        ErfRequest,
        undefined
    ),
    try
        ErfResponse = erf_router:handle(Name, ErfRequest),
        {CowboyReq2, StatusCode, Headers, Body} = postprocess(ErfRequest, ErfResponse, CowboyReq1),
        EndTime = erlang:monotonic_time(),
        ReqBodyLength = cowboy_req:body_length(CowboyReq0),
        RespBodyLength = resp_body_length(Body),
        Metrics = #{
            duration => EndTime - StartTime,
            monotonic_time => EndTime,
            req_body_duration => 0,
            req_body_length => case ReqBodyLength of undefined -> 0; L -> L end,
            resp_body_length => RespBodyLength,
            resp_duration => EndTime - StartTime
        },
        erf_telemetry:event({request_complete, Metrics}, Name, ErfRequest, {StatusCode, Headers, Body}),
        {ok, CowboyReq2, State}
    catch
        Class:Reason:Stacktrace ->
            handle_exception(Name, ErfRequest, Class, Reason, Stacktrace),
            CowboyReqErr = cowboy_req:reply(500, #{}, <<>>, CowboyReq1),
            {ok, CowboyReqErr, State}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec transport_opts(Port, Conf, SSL, NumAcceptors, MaxConnections) -> TransportOpts when
    Port :: inet:port_number(),
    Conf :: erf_http_server:conf(),
    SSL :: boolean(),
    NumAcceptors :: pos_integer(),
    MaxConnections :: pos_integer() | infinity,
    TransportOpts :: map().
transport_opts(Port, Conf, true, NumAcceptors, MaxConnections) ->
    CertFile = maps:get(certfile, Conf),
    KeyFile = maps:get(keyfile, Conf),
    #{
        socket_opts => [
            {port, Port},
            {certfile, CertFile},
            {keyfile, KeyFile}
        ],
        num_acceptors => NumAcceptors,
        max_connections => MaxConnections
    };
transport_opts(Port, _Conf, false, NumAcceptors, MaxConnections) ->
    #{
        socket_opts => [{port, Port}],
        num_acceptors => NumAcceptors,
        max_connections => MaxConnections
    }.

-spec preprocess(Name, Req) -> {Req, Request} when
    Name :: atom(),
    Req :: cowboy_req:req(),
    Request :: erf:request().
preprocess(Name, Req0) ->
    Scheme = cowboy_req:scheme(Req0),
    Host = cowboy_req:host(Req0),
    Port = cowboy_req:port(Req0),
    Path = cowboy_req:path_info(Req0),
    PathBin = cowboy_req:path(Req0),
    PathSegments =
        case Path of
            undefined ->
                % path_info is undefined if no [...] in route, parse manually
                [P || P <- binary:split(PathBin, <<"/">>, [global]), P =/= <<>>];
            P ->
                P
        end,
    Method = preprocess_method(cowboy_req:method(Req0)),
    QsVals = cowboy_req:parse_qs(Req0),
    Headers = maps:to_list(cowboy_req:headers(Req0)),
    Peer = peer_to_binary(cowboy_req:peer(Req0)),
    {Req1, RawBody} = read_body(Req0),
    JoinPath = erlang:iolist_to_binary([<<"/">>, lists:join(<<"/">>, PathSegments)]),
    Route =
        case erf:match_route(Name, JoinPath) of
            {ok, R} ->
                R;
            {error, not_found} ->
                JoinPath
        end,
    Request = #{
        scheme => Scheme,
        host => Host,
        port => Port,
        path => PathSegments,
        method => Method,
        query_parameters => QsVals,
        headers => Headers,
        body => RawBody,
        peer => Peer,
        route => Route
    },
    {Req1, Request}.

-spec preprocess_method(Method) -> Result when
    Method :: binary(),
    Result :: erf:method().
preprocess_method(<<"GET">>) ->
    get;
preprocess_method(<<"POST">>) ->
    post;
preprocess_method(<<"PUT">>) ->
    put;
preprocess_method(<<"DELETE">>) ->
    delete;
preprocess_method(<<"PATCH">>) ->
    patch;
preprocess_method(<<"HEAD">>) ->
    head;
preprocess_method(<<"OPTIONS">>) ->
    options;
preprocess_method(<<"TRACE">>) ->
    trace;
preprocess_method(<<"CONNECT">>) ->
    connect.

-spec read_body(Req) -> {Req, Body} when
    Req :: cowboy_req:req(),
    Body :: undefined | binary().
read_body(Req0) ->
    case cowboy_req:has_body(Req0) of
        false ->
            {Req0, undefined};
        true ->
            read_body_loop(Req0, <<>>)
    end.

-spec read_body_loop(Req, Acc) -> {Req, Body} when
    Req :: cowboy_req:req(),
    Acc :: binary(),
    Body :: undefined | binary().
read_body_loop(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req1} ->
            Body = <<Acc/binary, Data/binary>>,
            case Body of
                <<>> -> {Req1, undefined};
                _ -> {Req1, Body}
            end;
        {more, Data, Req1} ->
            read_body_loop(Req1, <<Acc/binary, Data/binary>>)
    end.

-spec peer_to_binary(Peer) -> Result when
    Peer :: {inet:ip_address(), inet:port_number()},
    Result :: binary().
peer_to_binary({IP, _Port}) ->
    erlang:list_to_binary(inet:ntoa(IP)).

-spec postprocess(Request, Response, Req) -> {Req, StatusCode, Headers, Body} when
    Request :: erf:request(),
    Response :: erf:response(),
    Req :: cowboy_req:req(),
    StatusCode :: pos_integer(),
    Headers :: [{binary(), binary()}],
    Body :: undefined | binary() | {file, binary()}.
postprocess(
    #{headers := ReqHeaders} = _Request,
    {Status, Headers, {file, File}},
    Req0
) ->
    HeadersMap = maps:from_list(Headers),
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            FileSize = element(2, FileInfo),
            Range = parse_range(ReqHeaders, FileSize),
            case Range of
                {partial, Start, End} ->
                    Length = End - Start + 1,
                    RangeHeaders = HeadersMap#{
                        <<"content-range">> => iolist_to_binary([
                            <<"bytes ">>,
                            integer_to_binary(Start),
                            <<"-">>,
                            integer_to_binary(End),
                            <<"/">>,
                            integer_to_binary(FileSize)
                        ]),
                        <<"content-length">> => integer_to_binary(Length)
                    },
                    Req1 = cowboy_req:reply(206, RangeHeaders, {sendfile, Start, Length, File}, Req0),
                    {Req1, 206, Headers, {file, File}};
                full ->
                    Req1 = cowboy_req:reply(Status, HeadersMap, {sendfile, 0, FileSize, File}, Req0),
                    {Req1, Status, Headers, {file, File}}
            end;
        {error, _Reason} ->
            Req1 = cowboy_req:reply(404, #{}, <<>>, Req0),
            {Req1, 404, [], undefined}
    end;
postprocess(_Request, {Status, Headers, Body}, Req0) ->
    HeadersMap = maps:from_list(Headers),
    RespBody = case Body of undefined -> <<>>; B -> B end,
    Req1 = cowboy_req:reply(Status, HeadersMap, RespBody, Req0),
    {Req1, Status, Headers, Body}.

-spec parse_range(Headers, FileSize) -> Result when
    Headers :: [{binary(), binary()}],
    FileSize :: non_neg_integer(),
    Result :: full | {partial, Start :: non_neg_integer(), End :: non_neg_integer()}.
parse_range(Headers, FileSize) ->
    case proplists:get_value(<<"range">>, Headers) of
        undefined ->
            full;
        RangeHeader ->
            case binary:match(RangeHeader, <<"bytes=">>) of
                {0, 6} ->
                    RangeSpec = binary:part(RangeHeader, 6, byte_size(RangeHeader) - 6),
                    parse_range_spec(RangeSpec, FileSize);
                _ ->
                    full
            end
    end.

-spec parse_range_spec(RangeSpec, FileSize) -> Result when
    RangeSpec :: binary(),
    FileSize :: non_neg_integer(),
    Result :: full | {partial, Start :: non_neg_integer(), End :: non_neg_integer()}.
parse_range_spec(RangeSpec, FileSize) ->
    case binary:split(RangeSpec, <<"-">>) of
        [<<>>, SuffixLength] ->
            % suffix-byte-range-spec: -500 means last 500 bytes
            Suffix = binary_to_integer(SuffixLength),
            Start = max(0, FileSize - Suffix),
            {partial, Start, FileSize - 1};
        [StartBin, <<>>] ->
            % byte-range-spec: 500- means from 500 to end
            Start = binary_to_integer(StartBin),
            {partial, Start, FileSize - 1};
        [StartBin, EndBin] ->
            Start = binary_to_integer(StartBin),
            End = min(binary_to_integer(EndBin), FileSize - 1),
            {partial, Start, End};
        _ ->
            full
    end.

-spec resp_body_length(Body) -> Length when
    Body :: undefined | binary() | {file, binary()},
    Length :: non_neg_integer().
resp_body_length(undefined) ->
    0;
resp_body_length({file, File}) ->
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            element(2, FileInfo);
        {error, _} ->
            0
    end;
resp_body_length(Body) when is_binary(Body) ->
    byte_size(Body).

-spec handle_exception(Name, Request, Class, Reason, Stacktrace) -> ok when
    Name :: atom(),
    Request :: erf:request(),
    Class :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: list().
handle_exception(Name, Request, Class, Reason, Stacktrace) ->
    {ok, LogLevel} = erf_conf:log_level(Name),
    ?LOG(LogLevel, "[erf] Request ~p ~p with exception ~p.~nStacktrace:~n~p", [
        Class, Request, Reason, Stacktrace
    ]),
    ExceptionData = #{
        error => erlang:list_to_binary(io_lib:format("~p", [Reason])),
        monotonic_time => erlang:monotonic_time(),
        stacktrace => erlang:list_to_binary(io_lib:format("~p", [Stacktrace]))
    },
    erf_telemetry:event({request_exception, ExceptionData}, Name, Request, {500, [], undefined}).
