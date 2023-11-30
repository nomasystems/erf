-module(users).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    users_sup:start_link().

stop(_State) ->
    ok.
