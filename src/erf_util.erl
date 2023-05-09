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
    to_camel_case/1,
    to_snake_case/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec to_camel_case(BinOrStr) -> CamelCase when
    BinOrStr :: binary() | string(),
    CamelCase :: binary().
%% @doc Naive function to convert a binary or string to CamelCase.
to_camel_case(Binary) when is_binary(Binary) ->
    to_camel_case(erlang:binary_to_list(Binary));
to_camel_case(String) ->
    to_camel_case(String, []).

-spec to_snake_case(BinOrStr) -> SnakeCase when
    BinOrStr :: binary() | string(),
    SnakeCase :: binary().
%% @doc Naive function to convert a binary or string to snake_case.
to_snake_case(Binary) when is_binary(Binary) ->
    to_snake_case(erlang:binary_to_list(Binary));
to_snake_case([C | _Rest] = List) when
    (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9)
->
    to_snake_case(List, []);
to_snake_case([C | Rest]) when C >= $A andalso C =< $Z ->
    to_snake_case([C + 32 | Rest], []);
to_snake_case([_C | Rest]) ->
    to_snake_case(Rest, []).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
to_camel_case([], Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc));
to_camel_case([$_, C | Rest], Acc) when C >= $a andalso C =< $z ->
    to_camel_case(Rest, [C - 32 | Acc]);
to_camel_case([C | Rest], Acc) when C >= $a andalso C =< $z ->
    to_camel_case(Rest, [C - 32 | Acc]);
to_camel_case([$_ | Rest], Acc) ->
    to_camel_case(Rest, Acc);
to_camel_case([C | Rest], Acc) ->
    to_camel_case(Rest, [C | Acc]).

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
