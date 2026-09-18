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

%% @doc <code>erf</code>'s RFC 9457 error formatter.
-module(erf_error_formatter_problem_json).

%%% BEHAVIOURS
-behaviour(erf_error_formatter).

%%% EXTERNAL EXPORTS
-export([
    format/1
]).

-ignore_xref([
    format/1
]).

%%% MACROS
-define(CONTENT_TYPE, <<"application/problem+json">>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec format(Error) -> Response when
    Error :: erf_error_formatter:error(),
    Response :: erf:response().
%% @doc Builds the response for an error, in the format
%% <a href="https://www.rfc-editor.org/rfc/rfc9457">RFC 9457</a> defines.
format({validation_failed, _Reason, Source}) ->
    problem(400, <<"Bad Request">>, detail(Source), []);
format(unreadable_body) ->
    problem(400, <<"Bad Request">>, <<"Failed to read request">>, []);
format(route_not_found) ->
    problem(404, <<"Not Found">>, <<"Route not found">>, []);
format({method_not_allowed, Methods}) ->
    Allow = lists:join(<<", ">>, [method(Method) || Method <- Methods]),
    problem(
        405,
        <<"Method Not Allowed">>,
        <<"Allowed methods are ", (erlang:iolist_to_binary(Allow))/binary>>,
        [{<<"allow">>, erlang:iolist_to_binary(Allow)}]
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec problem(Status, Title, Detail, Headers) -> Response when
    Status :: pos_integer(),
    Title :: binary(),
    Detail :: binary(),
    Headers :: [erf:header()],
    Response :: erf:response().
problem(Status, Title, Detail, Headers) ->
    Problem = #{
        <<"type">> => <<"about:blank">>,
        <<"title">> => Title,
        <<"status">> => Status,
        <<"detail">> => Detail
    },
    {Status, [{<<"content-type">>, ?CONTENT_TYPE} | Headers],
        erlang:iolist_to_binary(json:encode(Problem))}.

-spec method(Method) -> Name when
    Method :: erf:method(),
    Name :: binary().
method(get) ->
    <<"GET">>;
method(post) ->
    <<"POST">>;
method(put) ->
    <<"PUT">>;
method(delete) ->
    <<"DELETE">>;
method(patch) ->
    <<"PATCH">>;
method(head) ->
    <<"HEAD">>;
method(options) ->
    <<"OPTIONS">>;
method(trace) ->
    <<"TRACE">>;
method(connect) ->
    <<"CONNECT">>.

-spec detail(Source) -> Detail when
    Source :: erf_error_formatter:source() | undefined,
    Detail :: binary().
detail({body, undefined}) ->
    <<"Request body failed schema validation">>;
detail({In, Name}) ->
    <<(label(In))/binary, " parameter \"", Name/binary, "\" failed schema validation">>;
detail(undefined) ->
    <<"Request failed schema validation">>.

-spec label(In) -> Label when
    In :: erf_parser:parameter_type(),
    Label :: binary().
label(path) ->
    <<"Path">>;
label(query) ->
    <<"Query">>;
label(header) ->
    <<"Header">>;
label(cookie) ->
    <<"Cookie">>.
