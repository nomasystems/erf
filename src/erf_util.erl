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
-module(erf_util).

%%% EXTERNAL EXPORTS
-export([
    to_pascal_case/1,
    to_snake_case/1,
    handle_invalid_request/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec to_pascal_case(BinOrStr) -> PascalCase when
    BinOrStr :: binary() | string(),
    PascalCase :: binary().
%% @doc Naive function to convert a binary or string to PascalCase.
to_pascal_case(Binary) when is_binary(Binary) ->
    to_pascal_case(erlang:binary_to_list(Binary));
to_pascal_case([C | Rest]) when C >= $a andalso C =< $z ->
    to_pascal_case([C - 32 | Rest], []);
to_pascal_case(String) ->
    to_pascal_case(String, []).

-spec to_snake_case(BinOrStr) -> SnakeCase when
    BinOrStr :: binary() | string(),
    SnakeCase :: binary().
%% @doc Naive function to convert a binary or string to snake_case.
to_snake_case(Binary) when is_binary(Binary) ->
    to_snake_case(erlang:binary_to_list(Binary));
to_snake_case([C | Rest]) when
    (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9)
->
    to_snake_case(Rest, [C]);
to_snake_case([C | Rest]) when C >= $A andalso C =< $Z ->
    to_snake_case(Rest, [C + 32]);
to_snake_case([_C | Rest]) ->
    to_snake_case(Rest).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
to_pascal_case([], Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc));
to_pascal_case([C1, C2 | Rest], Acc) when
    ((C1 < $0) orelse (C1 > $9 andalso C1 < $A) orelse (C1 > $Z andalso C1 < $a) orelse (C1 > $z)) andalso
        (C2 >= $a andalso C2 =< $z)
->
    to_pascal_case(Rest, [C2 - 32 | Acc]);
to_pascal_case([C1, C2 | Rest], Acc) when
    (C1 >= $0 andalso C1 =< $9) andalso (C2 >= $a andalso C2 =< $z)
->
    to_pascal_case(Rest, [C2 - 32, C1 | Acc]);
to_pascal_case([C | Rest], Acc) when
    ((C < $0) orelse (C > $9 andalso C < $A) orelse (C > $Z andalso C < $a) orelse (C > $z))
->
    to_pascal_case(Rest, Acc);
to_pascal_case([C | Rest], Acc) ->
    to_pascal_case(Rest, [C | Acc]).

to_snake_case([], [$_ | Acc]) ->
    to_snake_case([], Acc);
to_snake_case([], Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc));
to_snake_case([C1, C2 | Rest], Acc) when
    ((C1 >= $a andalso C1 =< $z) orelse (C1 >= $0 andalso C1 =< $9)) andalso
        C2 >= $A andalso C2 =< $Z
->
    to_snake_case(Rest, [C2 + 32, $_, C1 | Acc]);
to_snake_case([C | Rest], Acc) when (C >= $a andalso C =< $z); (C >= $0 andalso C =< $9) ->
    to_snake_case(Rest, [C | Acc]);
to_snake_case([C | Rest], Acc) when C >= $A andalso C =< $Z ->
    to_snake_case(Rest, [C + 32 | Acc]);
to_snake_case([_C | Rest], [$_ | _T] = Acc) ->
    to_snake_case(Rest, Acc);
to_snake_case([_C | Rest], Acc) ->
    to_snake_case(Rest, [$_ | Acc]).


handle_invalid_request(_Request, Reason) ->
    {400, [{<<"content-type">>, <<"text/plain">>}], [io_lib:print(Reason), "\n"]}.
