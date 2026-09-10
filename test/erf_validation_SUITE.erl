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
%% limitations under the License.
-module(erf_validation_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(BODY_ONLY, {{body, undefined, true}}).
-define(BODY_PRESENT, {true}).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        body_pointer,
        nested_pointer,
        array_pointer,
        missing_property,
        missing_body,
        missing_parameter,
        parameter_pointer,
        keywords,
        no_value_echo,
        control_characters,
        oversized_detail,
        pointer_escaping,
        unknown_reason,
        error_cap
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
body_pointer(_Conf) ->
    Error = body_error(
        '$.properties.username.min_length', <<"String length \"Length\" is less than 3">>
    ),

    ?assertEqual(<<"body">>, maps:get(<<"in">>, Error)),
    ?assertEqual(<<"/username">>, maps:get(<<"pointer">>, Error)),
    ?assertEqual(<<"minLength">>, maps:get(<<"keyword">>, Error)),
    ?assertEqual(<<"String length is less than 3">>, maps:get(<<"detail">>, Error)),

    RootError = body_error('$.type', <<"Value is not an object">>),

    ?assertEqual(<<>>, maps:get(<<"pointer">>, RootError)),
    ?assertEqual(<<"type">>, maps:get(<<"keyword">>, RootError)),

    ok.

nested_pointer(_Conf) ->
    Error = body_error(
        '$.properties.address.properties.zip.pattern', <<"String does not match pattern ^[0-9]+$">>
    ),

    ?assertEqual(<<"/address/zip">>, maps:get(<<"pointer">>, Error)),
    ?assertEqual(<<"pattern">>, maps:get(<<"keyword">>, Error)),
    ?assertEqual(<<"String does not match pattern ^[0-9]+$">>, maps:get(<<"detail">>, Error)),

    ok.

array_pointer(_Conf) ->
    %% `ndto' numbers items from one in the message, JSON Pointer from zero.
    Error = body_error(
        '$.properties.tags.items[*].min_length',
        <<"Item 3 in $.properties.tags is invalid. String length \"Length\" is less than 2">>
    ),

    ?assertEqual(<<"/tags/2">>, maps:get(<<"pointer">>, Error)),
    ?assertEqual(<<"minLength">>, maps:get(<<"keyword">>, Error)),
    ?assertEqual(<<"String length is less than 2">>, maps:get(<<"detail">>, Error)),

    Nested = body_error(
        '$.properties.grid.items[*].items[*].type',
        <<"Item 1 in $.properties.grid is invalid. Item 2 in $.properties.grid.items[*] is invalid. Value is not an integer">>
    ),

    ?assertEqual(<<"/grid/0/1">>, maps:get(<<"pointer">>, Nested)),

    Tuple = body_error('$.properties.pair.items[1].type', <<"Value is not a string">>),

    ?assertEqual(<<"/pair/1">>, maps:get(<<"pointer">>, Tuple)),

    ok.

missing_property(_Conf) ->
    Error = body_error('$.', <<"$. is missing required property <<\"username\">>">>),

    ?assertEqual(<<"/username">>, maps:get(<<"pointer">>, Error)),
    ?assertEqual(<<"required">>, maps:get(<<"keyword">>, Error)),
    ?assertEqual(<<"Missing required property \"username\"">>, maps:get(<<"detail">>, Error)),

    Nested = body_error(
        '$.properties.address.',
        <<"$.properties.address. is missing required property <<\"zip\">>">>
    ),

    ?assertEqual(<<"/address/zip">>, maps:get(<<"pointer">>, Nested)),

    ok.

missing_body(_Conf) ->
    {400, Headers, Body} = erf_validation:bad_request(
        {{'$.type', <<"Value is not an object">>}, 0}, ?BODY_ONLY, {false}
    ),
    Problem = json:decode(Body),

    ?assertEqual(
        <<"application/problem+json">>, proplists:get_value(<<"content-type">>, Headers)
    ),
    ?assertEqual(<<"Request body is required">>, maps:get(<<"detail">>, Problem)),

    [Error] = maps:get(<<"errors">>, Problem),

    ?assertEqual(<<"required">>, maps:get(<<"keyword">>, Error)),
    ?assertEqual(<<>>, maps:get(<<"pointer">>, Error)),

    ok.

missing_parameter(_Conf) ->
    Sources = {{body, undefined, false}, {query, <<"limit">>, true}},
    {400, _Headers, Body} = erf_validation:bad_request(
        {{'$.type', <<"Value is not an integer">>}, 0}, Sources, {true, false}
    ),
    Problem = json:decode(Body),

    ?assertEqual(<<"Query parameter \"limit\" is required">>, maps:get(<<"detail">>, Problem)),

    [Error] = maps:get(<<"errors">>, Problem),

    ?assertEqual(<<"query">>, maps:get(<<"in">>, Error)),
    ?assertEqual(<<"/limit">>, maps:get(<<"pointer">>, Error)),
    ?assertEqual(<<"required">>, maps:get(<<"keyword">>, Error)),

    %% An absent optional parameter keeps the schema reason instead.
    Optional = {{body, undefined, false}, {query, <<"limit">>, false}},
    {400, _OptionalHeaders, OptionalBody} = erf_validation:bad_request(
        {{'$.minimum', <<"Value is not a number greater or equal to 1">>}, 0},
        Optional,
        {true, false}
    ),
    [OptionalError] = maps:get(<<"errors">>, json:decode(OptionalBody)),

    ?assertEqual(<<"minimum">>, maps:get(<<"keyword">>, OptionalError)),

    ok.

parameter_pointer(_Conf) ->
    Sources = {
        {body, undefined, false}, {path, <<"userId">>, true}, {header, <<"x-api-key">>, true}
    },

    PathError = single_error(
        {{'$.pattern', <<"String does not match pattern ^[0-9]+$">>}, 1},
        Sources,
        {true, true, true}
    ),

    ?assertEqual(<<"path">>, maps:get(<<"in">>, PathError)),
    ?assertEqual(<<"/userId">>, maps:get(<<"pointer">>, PathError)),
    ?assertEqual(<<"pattern">>, maps:get(<<"keyword">>, PathError)),

    HeaderError = single_error(
        {{'$.type', <<"Value is not a string">>}, 0}, Sources, {true, true, true}
    ),

    ?assertEqual(<<"header">>, maps:get(<<"in">>, HeaderError)),
    ?assertEqual(<<"/x-api-key">>, maps:get(<<"pointer">>, HeaderError)),

    BodyError = single_error(
        {{'$.type', <<"Value is not an object">>}, 2}, Sources, {true, true, true}
    ),

    ?assertEqual(<<"body">>, maps:get(<<"in">>, BodyError)),

    ok.

keywords(_Conf) ->
    Cases = [
        {'$.properties.a.max_length', <<"maxLength">>},
        {'$.properties.a.minimum', <<"minimum">>},
        {'$.properties.a.maximum', <<"maximum">>},
        {'$.properties.a.multiple_of', <<"multipleOf">>},
        {'$.properties.a.min_items', <<"minItems">>},
        {'$.properties.a.max_items', <<"maxItems">>},
        {'$.properties.a.unique_items', <<"uniqueItems">>},
        {'$.properties.a.min_properties', <<"minProperties">>},
        {'$.properties.a.max_properties', <<"maxProperties">>},
        {'$.properties.a.additional_properties', <<"additionalProperties">>},
        {'$.properties.a.unevaluated_properties', <<"unevaluatedProperties">>},
        {'$.properties.a.dependent_required', <<"dependentRequired">>},
        {'$.any_of', <<"anyOf">>},
        {'$.one_of', <<"oneOf">>},
        {'$.all_of', <<"allOf">>},
        {'$.properties.a.format', <<"format">>}
    ],
    lists:foreach(
        fun({SchemaPath, Keyword}) ->
            Error = body_error(SchemaPath, <<"Some reason">>),
            ?assertEqual(Keyword, maps:get(<<"keyword">>, Error))
        end,
        Cases
    ),

    Enum = body_error('$.properties.a', <<"Value is not one in the enum">>),

    ?assertEqual(<<"enum">>, maps:get(<<"keyword">>, Enum)),
    ?assertEqual(<<"/a">>, maps:get(<<"pointer">>, Enum)),

    ok.

no_value_echo(_Conf) ->
    %% The schema path and the message are the only inputs, and neither holds
    %% the value the caller sent.
    Error = body_error(
        '$.properties.password.min_length', <<"String length \"Length\" is less than 6">>
    ),
    Detail = maps:get(<<"detail">>, Error),

    ?assertEqual(nomatch, binary:match(Detail, <<"hunter2">>)),
    ?assertEqual(<<"/password">>, maps:get(<<"pointer">>, Error)),

    ok.

control_characters(_Conf) ->
    %% A property name reaches the response through `additionalProperties'.
    %% It must not carry a line break into a consumer log.
    Error = body_error(
        '$.additional_properties',
        <<"Object has unsupported keys: \"a\nb\rc\"">>
    ),
    Detail = maps:get(<<"detail">>, Error),

    ?assertEqual(nomatch, binary:match(Detail, <<"\n">>)),
    ?assertEqual(nomatch, binary:match(Detail, <<"\r">>)),

    Pointer = maps:get(
        <<"pointer">>,
        single_error(
            {{'$.type', <<"Value is not a string">>}, 0},
            {{query, <<"a\nb">>, false}},
            {true}
        )
    ),

    ?assertEqual(nomatch, binary:match(Pointer, <<"\n">>)),

    ok.

oversized_detail(_Conf) ->
    Long = binary:copy(<<"x">>, 5000),
    Error = body_error('$.type', <<"Object has unsupported keys: ", Long/binary>>),

    ?assert(byte_size(maps:get(<<"detail">>, Error)) =< 210),

    Utf8 = binary:copy(<<"ñ"/utf8>>, 500),
    Utf8Error = body_error('$.type', Utf8),
    Utf8Detail = maps:get(<<"detail">>, Utf8Error),

    ?assertNotEqual(error, unicode:characters_to_binary(Utf8Detail)),
    ?assert(is_binary(unicode:characters_to_binary(Utf8Detail))),

    ok.

pointer_escaping(_Conf) ->
    Error = body_error('$.properties.a/b.type', <<"Value is not a string">>),

    ?assertEqual(<<"/a~1b">>, maps:get(<<"pointer">>, Error)),

    Tilde = body_error('$.properties.a~b.type', <<"Value is not a string">>),

    ?assertEqual(<<"/a~0b">>, maps:get(<<"pointer">>, Tilde)),

    ok.

unknown_reason(_Conf) ->
    %% A validator that does not follow the `{SchemaPath, Message}' contract
    %% still gets a well formed problem document.
    {400, Headers, Body} = erf_validation:bad_request({reason, 0}, ?BODY_ONLY, ?BODY_PRESENT),
    Problem = json:decode(Body),

    ?assertEqual(
        <<"application/problem+json">>, proplists:get_value(<<"content-type">>, Headers)
    ),
    ?assertEqual(400, maps:get(<<"status">>, Problem)),
    ?assertEqual(<<"Bad Request">>, maps:get(<<"title">>, Problem)),
    ?assertEqual(<<"about:blank">>, maps:get(<<"type">>, Problem)),

    [Error] = maps:get(<<"errors">>, Problem),

    ?assertEqual(<<"Value failed schema validation">>, maps:get(<<"detail">>, Error)),
    ?assertEqual(false, maps:is_key(<<"keyword">>, Error)),

    %% An index that does not name a condition falls back to a generic body.
    {400, _OutOfRangeHeaders, OutOfRange} = erf_validation:bad_request(
        {{'$.type', <<"Value is not an object">>}, 99}, ?BODY_ONLY, ?BODY_PRESENT
    ),

    ?assertEqual(
        <<"Request failed schema validation">>, maps:get(<<"detail">>, json:decode(OutOfRange))
    ),

    {400, _BareHeaders, Bare} = erf_validation:bad_request(none_matched, ?BODY_ONLY, ?BODY_PRESENT),

    ?assertEqual(
        <<"Request failed schema validation">>, maps:get(<<"detail">>, json:decode(Bare))
    ),

    ok.

error_cap(_Conf) ->
    %% `ndto' short circuits, so the list holds one entry today.
    {400, _Headers, Body} = erf_validation:bad_request(
        {{'$.type', <<"Value is not an object">>}, 0}, ?BODY_ONLY, ?BODY_PRESENT
    ),

    ?assertEqual(1, erlang:length(maps:get(<<"errors">>, json:decode(Body)))),

    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
body_error(SchemaPath, Message) ->
    single_error({{SchemaPath, Message}, 0}, ?BODY_ONLY, ?BODY_PRESENT).

single_error(Reason, Sources, Presence) ->
    {400, _Headers, Body} = erf_validation:bad_request(Reason, Sources, Presence),
    [Error] = maps:get(<<"errors">>, json:decode(Body)),
    Error.
