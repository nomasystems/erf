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

%% @doc <code>erf</code>'s request validation error module.
-module(erf_validation).

%%% EXTERNAL EXPORTS
-export([
    bad_request/2
]).

-ignore_xref([
    bad_request/2
]).

%%% TYPES
-type source() :: {in(), Name :: binary() | undefined}.
-type in() :: body | path | query | header | cookie.

%%% TYPE EXPORTS
-export_type([
    in/0,
    source/0
]).

%%% MACROS
-define(CONTENT_TYPE, <<"application/problem+json">>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec bad_request(Reason, Sources) -> Response when
    Reason :: term(),
    Sources :: tuple(),
    Response :: erf:response().
%% @doc Builds the <code>400</code> response for a request that failed validation.
%%
%% <code>Reason</code> is the term returned by <code>ndto_validation:'andalso'/1</code> and
%% <code>Sources</code> a tuple describing, in the order they are validated, the part of the
%% request each condition covers. The generated router holds both.
bad_request(Reason, Sources) ->
    Problem = #{
        <<"type">> => <<"about:blank">>,
        <<"title">> => <<"Bad Request">>,
        <<"status">> => 400,
        <<"detail">> => detail(source(Reason, Sources))
    },
    {400, [{<<"content-type">>, ?CONTENT_TYPE}], erlang:iolist_to_binary(json:encode(Problem))}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec source(Reason, Sources) -> Source when
    Reason :: term(),
    Sources :: tuple(),
    Source :: source() | undefined.
%% @doc <code>ndto_validation:'andalso'/1</code> counts its conditions down from
%% <code>length(Conditions) - 1</code>, so the condition at position <code>P</code>, starting
%% at one, reports index <code>Size - P</code>.
source({_RawReason, Index}, Sources) when is_integer(Index), is_tuple(Sources) ->
    source_at(erlang:tuple_size(Sources) - Index, Sources);
source(_Reason, _Sources) ->
    undefined.

-spec source_at(Position, Sources) -> Source when
    Position :: integer(),
    Sources :: tuple(),
    Source :: source() | undefined.
source_at(Position, Sources) when Position >= 1, Position =< tuple_size(Sources) ->
    erlang:element(Position, Sources);
source_at(_Position, _Sources) ->
    undefined.

-spec detail(Source) -> Detail when
    Source :: source() | undefined,
    Detail :: binary().
detail({body, _Name}) ->
    <<"Request body failed schema validation">>;
detail({In, Name}) when is_binary(Name) ->
    <<(label(In))/binary, " parameter \"", Name/binary, "\" failed schema validation">>;
detail(_Source) ->
    <<"Request failed schema validation">>.

-spec label(In) -> Label when
    In :: path | query | header | cookie,
    Label :: binary().
label(path) ->
    <<"Path">>;
label(query) ->
    <<"Query">>;
label(header) ->
    <<"Header">>;
label(cookie) ->
    <<"Cookie">>.
