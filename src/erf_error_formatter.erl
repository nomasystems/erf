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

%% @doc Behaviour for <code>erf</code>'s error formatters.
-module(erf_error_formatter).

%%% TYPES
-type t() :: module().
% A module that implements this behaviour.
-type source() :: {body, undefined} | {erf_parser:parameter_type(), Name :: binary()}.
% The part of the request that failed validation.
-type error() ::
    {validation_failed, Reason :: term(), Source :: erf_error_formatter:source() | undefined}
    | unreadable_body
    | route_not_found
    | {method_not_allowed, Methods :: [erf:method()]}.

%%% TYPE EXPORTS
-export_type([
    error/0,
    source/0,
    t/0
]).

%%%-----------------------------------------------------------------------------
%%% CALLBACKS
%%%-----------------------------------------------------------------------------
-callback format(Error) -> Result when
    Error :: error(),
    Result :: erf:response() | default.
% Returning <code>default</code> leaves the error to <code>erf</code>, which answers it with an
% empty body.
