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

%% @doc <code>erf</code>'s interface to interact with its underlying HTTP server.
-module(erf_http_server).

%%% EXTERNAL EXPORTS
-export([
    start_link/4
]).

%%% TYPES
-type conf() :: #{
    port => inet:port_number(),
    ssl => boolean(),
    certfile => binary(),
    keyfile => binary()
}.
-type t() :: module().
-type extra_conf() :: term().

%%% TYPE EXPORTS
-export_type([
    conf/0,
    t/0,
    extra_conf/0
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback start_link(Name, Conf, ExtraConf) -> Result when
    Name :: atom(),
    Conf :: conf(),
    ExtraConf :: extra_conf(),
    Result :: supervisor:startlink_ret().

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(Module, ModuleExtraConf, Name, Conf) -> Result when
    Module :: module(),
    ModuleConf :: extra_conf(),
    Name :: atom(),
    Conf :: conf(),
    Result :: supervisor:startlink_ret().
start_link(Module, ModuleExtraConf, Name, Conf) ->
    Module:start_link(Name, Conf, ModuleExtraConf).
