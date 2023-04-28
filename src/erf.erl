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
-module(erf).

%%% START/STOP EXPORTS
-export([
    start_link/1
]).

%%% TYPES
-type api() :: erf_parser:api().
-type conf() :: #{
    spec := binary(),
    callback => module(),
    port => inet:port_number(),
    ssl => boolean(),
    keyfile => binary(),
    certfile => binary()
}.

%%% TYPE EXPORTS
-export_type([
    api/0,
    conf/0
]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(Conf) -> Result when
    Conf :: conf(),
    Result :: {ok, Pid} | ignore | {error, Reason},
    Pid :: pid(),
    Reason :: term().
%% @doc Starts the supervision tree for an instance of the server.
start_link(_Conf) ->
    erlang:throw({not_implemented, 'start_link/1'}).
