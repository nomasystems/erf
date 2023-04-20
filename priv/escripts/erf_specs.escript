#!/usr/bin/env escript
%%! -pa _build/default/lib/njson/ebin -pa _build/default/lib/ndto/ebin

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
-module(erf_specs).

%%% ESCRIPT ENTRY POINT
-export([main/1]).

%%%-----------------------------------------------------------------------------
%%% ESCRIPT ENTRY POINT
%%%-----------------------------------------------------------------------------
main([]) ->
    io:format("===> Generating DTOs from specs...~n"),
    oas_3_0().


%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
oas_3_0() ->
    {ok, Bin} = file:read_file("priv/oas-spec/oas_3_0.json"),
    JSONSchema = njson:decode(Bin),
    Definitions = maps:to_list(maps:get(<<"definitions">>, JSONSchema, #{})),
    filelib:ensure_dir("_gen/dtos/"),
    lists:foreach(
        fun({Name, Schema}) ->
            DTO = ndto:generate(erlang:binary_to_atom(Name), Schema),
            ndto:write(DTO, <<"_gen/dtos/", Name/binary, ".erl">>)
        end,
        [{<<"oas_3_0">>, JSONSchema} | Definitions]
    ).
