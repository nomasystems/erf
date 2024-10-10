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

%% <code>erf</code>'s telemetry module.
-module(erf_telemetry).

%%% EXTERNAL EXPORTS
-export([
    event/4
]).

%%% TYPES
-type event() ::
    {request_start, #{monotonic_time := monotonic_time()}}
    | {request_complete, req_measurements()}
    | {request_exception, exception_data()}.

-type exception_data() :: #{
    error := binary(),
    monotonic_time := monotonic_time(),
    stacktrace => binary()
}.
-type monotonic_time() :: integer().
-type req_measurements() :: #{
    duration := integer(),
    monotonic_time := monotonic_time(),
    req_body_duration => integer(),
    req_body_length => integer(),
    resp_body_length => integer(),
    resp_duration := integer()
}.

%%% TYPE EXPORTS
-export_type([
    event/0,
    exception_data/0,
    monotonic_time/0,
    req_measurements/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec event(Event, Name, Req, Resp) -> ok when
    Event :: event(),
    Name :: atom(),
    Req :: erf:request(),
    Resp :: undefined | erf:response().
event({request_exception, ExceptionData} = Event, Name, Req, Resp) ->
    case code:is_loaded(telemetry) of
        {file, _TelemetryBeam} ->
            telemetry:execute(
                metric(Event),
                [],
                metadata(Name, Req, Resp, ExceptionData)
            );
        _Error ->
            ok
    end;
event({_EventName, Measurements} = Event, Name, Req, Resp) ->
    case code:is_loaded(telemetry) of
        {file, _TelemetryBeam} ->
            telemetry:execute(
                metric(Event),
                Measurements,
                metadata(Name, Req, Resp, #{})
            );
        _Error ->
            ok
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
metadata(Name, Req, undefined, RawMetadata) ->
    RawMetadata#{
        name => Name,
        req => Req
    };
metadata(Name, Req, {RespStatus, RespHeaders, _Body}, RawMetadata) ->
    RawMetadata#{
        name => Name,
        req => Req,
        resp_headers => RespHeaders,
        resp_status => RespStatus
    }.

metric({request_complete, _Measurements}) ->
    [erf, request, stop];
metric({request_exception, _ExceptionData}) ->
    [erf, request, fail];
metric({request_start, _Measurements}) ->
    [erf, request, start].
