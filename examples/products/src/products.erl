-module(products).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    products_sup:start_link().

stop(_State) ->
    ok.
