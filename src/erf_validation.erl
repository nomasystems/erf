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
-module(erf_validation).
-moduledoc """
Builds the RFC 9457 `application/problem+json` body that `erf` returns when a
request fails schema validation.

The generated router calls `bad_request/3` from the failure branch of its
validation `case`. Every argument is either a compile-time literal or a value
the router already holds, so the happy path pays nothing for this module.

`ndto` reports a failure as `{SchemaPath, Message}`. `SchemaPath` is an atom in
`ndto`'s internal grammar, such as `'$.properties.username.min_length'`. This
module translates it into a JSON Pointer (RFC 6901) and a JSON Schema keyword,
and never puts the atom itself in the response.

The response never holds a value that the caller sent. `ndto` messages name
schema constraints, and for a small set of keywords (`additionalProperties`,
`patternProperties`, `propertyNames`, `unevaluatedProperties`) they also name
object keys. Keys reach the response through `sanitize/1`, which removes
control characters and caps the length, so a key cannot inject a line break
into a consumer log or grow the response.
""".

%%% EXTERNAL EXPORTS
-export([
    bad_request/3
]).

-ignore_xref([
    bad_request/3
]).

%%% TYPES
-type in() :: body | path | query | header | cookie.
-type source() :: {in(), binary() | undefined, Required :: boolean()}.
-type reason() :: {SchemaPath :: atom(), Message :: binary()}.

%%% TYPE EXPORTS
-export_type([
    in/0,
    source/0
]).

%%% MACROS
-define(MAX_ERRORS, 10).
-define(MAX_DETAIL, 200).
-define(MAX_SEGMENT, 100).
-define(CONTENT_TYPE, <<"application/problem+json">>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec bad_request(Reason, Sources, Presence) -> Response when
    Reason :: term(),
    Sources :: tuple(),
    Presence :: tuple(),
    Response :: erf:response().
-doc """
Builds the `400` response for a failed request validation.

`Reason` is the term `ndto_validation:'andalso'/1` returned. `Sources` is a
literal tuple that describes each validated condition in condition order, and
`Presence` is a tuple of booleans that tells, for the same conditions, whether
the caller sent the value at all. Both tuples are produced by the generated
router.
""".
bad_request(Reason, Sources, Presence) ->
    {Source, Present, Detail} = locate(Reason, Sources, Presence),
    Problem = #{
        <<"type">> => <<"about:blank">>,
        <<"title">> => <<"Bad Request">>,
        <<"status">> => 400,
        <<"detail">> => summary(Source, Present),
        <<"errors">> => cap(errors(Source, Present, Detail))
    },
    {400, [{<<"content-type">>, ?CONTENT_TYPE}], erlang:iolist_to_binary(json:encode(Problem))}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec locate(Reason, Sources, Presence) -> Result when
    Reason :: term(),
    Sources :: tuple(),
    Presence :: tuple(),
    Result :: {source() | undefined, boolean(), reason() | undefined}.
%% `ndto_validation:'andalso'/1' counts its conditions down from
%% `length(Conditions) - 1', so the condition at position `P' (one-based)
%% reports index `Size - P'.
locate({RawReason, Index}, Sources, Presence) when
    is_integer(Index), is_tuple(Sources), is_tuple(Presence)
->
    at(erlang:tuple_size(Sources) - Index, Sources, Presence, detail(RawReason));
locate(RawReason, _Sources, _Presence) ->
    {undefined, true, detail(RawReason)}.

-spec at(Position, Sources, Presence, Detail) -> Result when
    Position :: integer(),
    Sources :: tuple(),
    Presence :: tuple(),
    Detail :: reason() | undefined,
    Result :: {source() | undefined, boolean(), reason() | undefined}.
at(Position, Sources, Presence, Detail) when
    Position >= 1,
    Position =< tuple_size(Sources),
    Position =< tuple_size(Presence)
->
    {erlang:element(Position, Sources), erlang:element(Position, Presence) =/= false, Detail};
at(_Position, _Sources, _Presence, Detail) ->
    {undefined, true, Detail}.

-spec detail(RawReason) -> Result when
    RawReason :: term(),
    Result :: reason() | undefined.
detail({SchemaPath, Message}) when is_atom(SchemaPath), is_binary(Message) ->
    {SchemaPath, Message};
detail(_RawReason) ->
    undefined.

-spec summary(Source, Present) -> Detail when
    Source :: source() | undefined,
    Present :: boolean(),
    Detail :: binary().
summary({body, _Name, true}, false) ->
    <<"Request body is required">>;
summary({body, _Name, _Required}, _Present) ->
    <<"Request body failed schema validation">>;
summary({In, Name, true}, false) when is_binary(Name) ->
    <<(label(In))/binary, " \"", (segment(Name))/binary, "\" is required">>;
summary({In, Name, _Required}, _Present) when is_binary(Name) ->
    <<(label(In))/binary, " \"", (segment(Name))/binary, "\" failed schema validation">>;
summary(_Source, _Present) ->
    <<"Request failed schema validation">>.

-spec label(In) -> Label when
    In :: path | query | header | cookie,
    Label :: binary().
label(path) -> <<"Path parameter">>;
label(query) -> <<"Query parameter">>;
label(header) -> <<"Header">>;
label(cookie) -> <<"Cookie">>.

-spec errors(Source, Present, Detail) -> Errors when
    Source :: source() | undefined,
    Present :: boolean(),
    Detail :: reason() | undefined,
    Errors :: [map()].
errors({In, Name, true} = Source, false, _Detail) ->
    [
        error_entry(
            In, pointer(root(Source), []), <<"required">>, summary({In, Name, true}, false)
        )
    ];
errors(Source, _Present, {SchemaPath, Message}) ->
    {Segments, Keyword, Detail} = explain(SchemaPath, Message),
    [error_entry(location(Source), pointer(root(Source), Segments), Keyword, Detail)];
errors(Source, _Present, undefined) ->
    [
        error_entry(
            location(Source),
            pointer(root(Source), []),
            undefined,
            <<"Value failed schema validation">>
        )
    ].

-spec error_entry(In, Pointer, Keyword, Detail) -> Entry when
    In :: in() | undefined,
    Pointer :: binary(),
    Keyword :: binary() | undefined,
    Detail :: binary(),
    Entry :: map().
error_entry(In, Pointer, Keyword, Detail) ->
    Entry = #{<<"pointer">> => Pointer, <<"detail">> => Detail},
    WithIn =
        case In of
            undefined -> Entry;
            _In -> Entry#{<<"in">> => erlang:atom_to_binary(In, utf8)}
        end,
    case Keyword of
        undefined -> WithIn;
        _Keyword -> WithIn#{<<"keyword">> => Keyword}
    end.

-spec location(Source) -> In when
    Source :: source() | undefined,
    In :: in() | undefined.
location({In, _Name, _Required}) -> In;
location(undefined) -> undefined.

-spec root(Source) -> Segments when
    Source :: source() | undefined,
    Segments :: [binary()].
%% A body failure is rooted at the document, a parameter failure at a pointer
%% that names the parameter, so both shapes carry the failing field.
root({body, _Name, _Required}) -> [];
root({_In, Name, _Required}) when is_binary(Name) -> [Name];
root(_Source) -> [].

-spec cap(Errors) -> Errors when
    Errors :: [map()].
%% `ndto' short-circuits on the first failure, so today this list holds one
%% entry. The cap keeps the response bounded if that ever changes.
cap(Errors) ->
    lists:sublist(Errors, ?MAX_ERRORS).

%%%-----------------------------------------------------------------------------
%%% SCHEMA PATH TRANSLATION
%%%-----------------------------------------------------------------------------
-spec explain(SchemaPath, Message) -> Result when
    SchemaPath :: atom(),
    Message :: binary(),
    Result :: {Segments :: [binary()], Keyword :: binary() | undefined, Detail :: binary()}.
explain(SchemaPath, Message) ->
    {Indexes, Inner} = strip_items(Message, []),
    {Segments, Keyword} = walk_path(erlang:atom_to_binary(SchemaPath, utf8), Indexes),
    case required_property(Inner) of
        {ok, Property} ->
            {
                Segments ++ [Property],
                <<"required">>,
                <<"Missing required property \"", (segment(Property))/binary, "\"">>
            };
        error ->
            {Segments, keyword_fallback(Keyword, Inner), sanitize(Inner)}
    end.

-spec keyword_fallback(Keyword, Message) -> Keyword when
    Keyword :: binary() | undefined,
    Message :: binary().
%% `ndto' names an `enum' validator after the schema it guards, so the path
%% carries no keyword. The message is the only place the keyword survives.
keyword_fallback(undefined, <<"Value is not one in the enum">>) ->
    <<"enum">>;
keyword_fallback(Keyword, _Message) ->
    Keyword.

-spec walk_path(Path, Indexes) -> Result when
    Path :: binary(),
    Indexes :: [binary()],
    Result :: {Segments :: [binary()], Keyword :: binary() | undefined}.
walk_path(<<"$">>, _Indexes) ->
    {[], undefined};
walk_path(<<"$.", Rest/binary>>, Indexes) ->
    walk(binary:split(Rest, <<".">>, [global]), Indexes, [], undefined);
walk_path(_Path, _Indexes) ->
    {[], undefined}.

-spec walk(Tokens, Indexes, Segments, Keyword) -> Result when
    Tokens :: [binary()],
    Indexes :: [binary()],
    Segments :: [binary()],
    Keyword :: binary() | undefined,
    Result :: {[binary()], binary() | undefined}.
walk([], _Indexes, Segments, Keyword) ->
    {lists:reverse(Segments), Keyword};
walk([<<"properties">> | Rest], Indexes, Segments, _Keyword) ->
    {Name, Tail} = property_name(Rest),
    walk(Tail, Indexes, [Name | Segments], undefined);
walk([Token | Rest], Indexes, Segments, Keyword) ->
    case split_bracket(Token) of
        {<<"items">>, <<"*">>} ->
            case Indexes of
                [Index | Tail] ->
                    walk(Rest, Tail, [Index | Segments], undefined);
                [] ->
                    walk(Rest, Indexes, Segments, <<"items">>)
            end;
        {<<"items">>, Index} ->
            case is_index(Index) of
                true -> walk(Rest, Indexes, [Index | Segments], undefined);
                false -> walk(Rest, Indexes, Segments, <<"items">>)
            end;
        {Base, _Bracket} ->
            walk(Rest, Indexes, Segments, keyword(Base, Keyword))
    end.

-spec property_name(Tokens) -> Result when
    Tokens :: [binary()],
    Result :: {Name :: binary(), Rest :: [binary()]}.
%% A property name is one token, unless the name itself holds a dot. Nested
%% properties always go through another `properties' token, so extending the
%% name until the next grammar token cannot swallow a nested level.
property_name([]) ->
    {<<>>, []};
property_name([Token | Rest]) ->
    property_name(Rest, [Token]).

property_name([], Acc) ->
    {join(Acc), []};
property_name([Token | Rest] = Tokens, Acc) ->
    case structural(Token) of
        true -> {join(Acc), Tokens};
        false -> property_name(Rest, [Token | Acc])
    end.

-spec join(Reversed) -> Name when
    Reversed :: [binary()],
    Name :: binary().
join(Reversed) ->
    erlang:iolist_to_binary(lists:join(<<".">>, lists:reverse(Reversed))).

-spec split_bracket(Token) -> Result when
    Token :: binary(),
    Result :: {Base :: binary(), Bracket :: binary()}.
split_bracket(Token) ->
    Size = erlang:byte_size(Token),
    case binary:match(Token, <<"[">>) of
        {Position, 1} when Size > Position + 1 ->
            case binary:last(Token) of
                $] ->
                    {
                        binary:part(Token, 0, Position),
                        binary:part(Token, Position + 1, Size - Position - 2)
                    };
                _Other ->
                    {Token, <<>>}
            end;
        _Other ->
            {Token, <<>>}
    end.

-spec is_index(Bracket) -> boolean() when
    Bracket :: binary().
is_index(<<>>) ->
    false;
is_index(Bracket) ->
    lists:all(fun(Byte) -> Byte >= $0 andalso Byte =< $9 end, erlang:binary_to_list(Bracket)).

-spec structural(Token) -> boolean() when
    Token :: binary().
%% Tokens that belong to `ndto''s path grammar rather than to a property name.
structural(<<>>) ->
    true;
structural(<<"*">>) ->
    true;
structural(<<"schema">>) ->
    true;
structural(Token) ->
    {Base, _Bracket} = split_bracket(Token),
    keyword(Base, undefined) =/= undefined.

-spec keyword(Token, Previous) -> Keyword when
    Token :: binary(),
    Previous :: binary() | undefined,
    Keyword :: binary() | undefined.
keyword(<<"type">>, _Previous) -> <<"type">>;
keyword(<<"enum">>, _Previous) -> <<"enum">>;
keyword(<<"format">>, _Previous) -> <<"format">>;
keyword(<<"pattern">>, _Previous) -> <<"pattern">>;
keyword(<<"min_length">>, _Previous) -> <<"minLength">>;
keyword(<<"max_length">>, _Previous) -> <<"maxLength">>;
keyword(<<"minimum">>, _Previous) -> <<"minimum">>;
keyword(<<"maximum">>, _Previous) -> <<"maximum">>;
keyword(<<"multiple_of">>, _Previous) -> <<"multipleOf">>;
keyword(<<"items">>, _Previous) -> <<"items">>;
keyword(<<"min_items">>, _Previous) -> <<"minItems">>;
keyword(<<"max_items">>, _Previous) -> <<"maxItems">>;
keyword(<<"unique_items">>, _Previous) -> <<"uniqueItems">>;
keyword(<<"unevaluated_items">>, _Previous) -> <<"unevaluatedItems">>;
keyword(<<"properties">>, _Previous) -> <<"properties">>;
keyword(<<"required">>, _Previous) -> <<"required">>;
keyword(<<"min_properties">>, _Previous) -> <<"minProperties">>;
keyword(<<"max_properties">>, _Previous) -> <<"maxProperties">>;
keyword(<<"pattern_properties">>, _Previous) -> <<"patternProperties">>;
keyword(<<"additional_properties">>, _Previous) -> <<"additionalProperties">>;
keyword(<<"property_names">>, _Previous) -> <<"propertyNames">>;
keyword(<<"dependent_required">>, _Previous) -> <<"dependentRequired">>;
keyword(<<"dependent_schemas">>, _Previous) -> <<"dependentSchemas">>;
keyword(<<"unevaluated_properties">>, _Previous) -> <<"unevaluatedProperties">>;
keyword(<<"all_of">>, _Previous) -> <<"allOf">>;
keyword(<<"any_of">>, _Previous) -> <<"anyOf">>;
keyword(<<"one_of">>, _Previous) -> <<"oneOf">>;
keyword(<<"not">>, _Previous) -> <<"not">>;
keyword(<<"if">>, _Previous) -> <<"if">>;
keyword(<<"then">>, _Previous) -> <<"then">>;
keyword(<<"else">>, _Previous) -> <<"else">>;
keyword(<<"ref">>, _Previous) -> <<"$ref">>;
keyword(_Token, Previous) -> Previous.

%%%-----------------------------------------------------------------------------
%%% MESSAGE TRANSLATION
%%%-----------------------------------------------------------------------------
-spec strip_items(Message, Acc) -> Result when
    Message :: binary(),
    Acc :: [binary()],
    Result :: {Indexes :: [binary()], Inner :: binary()}.
%% `ndto' reports an array failure as `Item N in <path> is invalid. <inner>',
%% with `N' one-based and one layer per nesting level. The indexes belong in
%% the JSON Pointer, so peel them off here and hand the innermost message on.
strip_items(<<"Item ", Rest/binary>> = Message, Acc) ->
    case binary:split(Rest, <<" is invalid. ">>) of
        [Head, Tail] ->
            case binary:split(Head, <<" in ">>) of
                [Raw, _Where] ->
                    case zero_based(Raw) of
                        {ok, Index} -> strip_items(Tail, [Index | Acc]);
                        error -> {lists:reverse(Acc), Message}
                    end;
                _Other ->
                    {lists:reverse(Acc), Message}
            end;
        _Other ->
            {lists:reverse(Acc), Message}
    end;
strip_items(Message, Acc) ->
    {lists:reverse(Acc), Message}.

-spec zero_based(Raw) -> Result when
    Raw :: binary(),
    Result :: {ok, binary()} | error.
zero_based(Raw) ->
    try erlang:binary_to_integer(Raw) of
        Index when Index >= 1 -> {ok, erlang:integer_to_binary(Index - 1)};
        Index when Index >= 0 -> {ok, erlang:integer_to_binary(Index)};
        _Negative -> error
    catch
        error:badarg -> error
    end.

-spec required_property(Message) -> Result when
    Message :: binary(),
    Result :: {ok, binary()} | error.
required_property(Message) ->
    case binary:split(Message, <<" is missing required property ">>) of
        [_Prefix, Raw] -> {ok, unquote(Raw)};
        _Other -> error
    end.

-spec unquote(Raw) -> Property when
    Raw :: binary(),
    Property :: binary().
%% `ndto' prints the missing property with `~p', so it arrives as `<<"name">>'.
unquote(<<"<<\"", Rest/binary>>) ->
    case binary:match(Rest, <<"\">>">>) of
        {Position, _Length} -> binary:part(Rest, 0, Position);
        nomatch -> Rest
    end;
unquote(Raw) ->
    Raw.

-spec sanitize(Message) -> Detail when
    Message :: binary(),
    Detail :: binary().
sanitize(Message) ->
    Stripped = strip_path_prefix(Message),
    Unquoted = binary:replace(
        binary:replace(Stripped, <<"<<\"">>, <<"\"">>, [global]), <<"\">>">>, <<"\"">>, [global]
    ),
    %% `ndto' emits the literal text `"Length"' where the measured length belongs.
    Repaired = binary:replace(Unquoted, <<" \"Length\"">>, <<>>, [global]),
    truncate(printable(capitalize(Repaired)), ?MAX_DETAIL).

-spec strip_path_prefix(Message) -> Message when
    Message :: binary().
%% Some `ndto' messages open with the schema path. That path is the internal
%% grammar this module exists to hide, so drop it.
strip_path_prefix(<<"$", _Rest/binary>> = Message) ->
    case binary:split(Message, <<" ">>) of
        [_Path, Tail] -> Tail;
        _Other -> Message
    end;
strip_path_prefix(Message) ->
    Message.

-spec capitalize(Message) -> Message when
    Message :: binary().
capitalize(<<Byte, Rest/binary>>) when Byte >= $a, Byte =< $z ->
    <<(Byte - 32), Rest/binary>>;
capitalize(Message) ->
    Message.

-spec segment(Raw) -> Segment when
    Raw :: binary(),
    Segment :: binary().
segment(Raw) ->
    truncate(printable(Raw), ?MAX_SEGMENT).

-spec printable(Raw) -> Clean when
    Raw :: binary(),
    Clean :: binary().
%% Object keys reach the response through `ndto' messages and pointers. A
%% control character in a key must not become a line break in a consumer log.
printable(Raw) ->
    printable(Raw, <<>>).

-spec printable(Raw, Acc) -> Clean when
    Raw :: binary(),
    Acc :: binary(),
    Clean :: binary().
printable(<<>>, Acc) ->
    Acc;
printable(<<Byte, Rest/binary>>, Acc) when Byte < 16#20; Byte =:= 16#7F ->
    printable(Rest, <<Acc/binary, $\s>>);
printable(<<Byte, Rest/binary>>, Acc) ->
    printable(Rest, <<Acc/binary, Byte>>).

-spec truncate(Raw, Max) -> Result when
    Raw :: binary(),
    Max :: pos_integer(),
    Result :: binary().
truncate(Raw, Max) when erlang:byte_size(Raw) =< Max ->
    Raw;
truncate(Raw, Max) ->
    <<(binary:part(Raw, 0, safe_cut(Raw, Max)))/binary, "...">>.

-spec safe_cut(Raw, Position) -> Position when
    Raw :: binary(),
    Position :: non_neg_integer().
%% Never cut inside a UTF-8 sequence: back off over continuation bytes.
safe_cut(_Raw, 0) ->
    0;
safe_cut(Raw, Position) ->
    case binary:at(Raw, Position) of
        Byte when Byte >= 16#80, Byte < 16#C0 -> safe_cut(Raw, Position - 1);
        _Byte -> Position
    end.

%%%-----------------------------------------------------------------------------
%%% JSON POINTER
%%%-----------------------------------------------------------------------------
-spec pointer(Root, Segments) -> Pointer when
    Root :: [binary()],
    Segments :: [binary()],
    Pointer :: binary().
pointer(Root, Segments) ->
    erlang:iolist_to_binary([[<<"/">>, escape(segment(Part))] || Part <- Root ++ Segments]).

-spec escape(Segment) -> Escaped when
    Segment :: binary(),
    Escaped :: binary().
escape(Segment) ->
    binary:replace(
        binary:replace(Segment, <<"~">>, <<"~0">>, [global]), <<"/">>, <<"~1">>, [global]
    ).
