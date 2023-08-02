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

%% @doc Behaviour for <code>erf</code>'s postprocessing middlewares.
-module(erf_postprocess_middleware).

%%% TYPES
-type t() :: module().
% A module that implements this behaviour.

%%% TYPE EXPORTS
-export_type([
    t/0
]).

%%%-----------------------------------------------------------------------------
%%% CALLBACKS
%%%-----------------------------------------------------------------------------
-callback postprocess(Request, Response) -> Result when
    Request :: erf:request(),
    Response :: erf:response(),
    Result :: erf:response().
