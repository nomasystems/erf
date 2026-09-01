-module(shop_sup).

%%% BEHAVIOURS
-behaviour(supervisor).

%%% START/STOP EXPORTS
-export([start_link/0]).

%%% INTERNAL EXPORTS
-export([init/1]).

%%%-------------------------------------------------------
%%% START/STOP EXPORTS
%%%-------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%-------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-------------------------------------------------------
init([]) ->
    ets:new(orders, [public, named_table]),
    ets:new(products, [public, named_table]),
    ShopAPIConf = #{
        mounts => [
            #{
                base_path => <<"/">>,
                spec_path => filename:join(code:priv_dir(shop), <<"orders.openapi.json">>),
                callback => shop_orders_callback
            },
            #{
                base_path => <<"/catalog">>,
                spec_path => filename:join(code:priv_dir(shop), <<"catalog.openapi.json">>),
                callback => shop_catalog_callback
            }
        ],
        swagger_ui => true,
        port => 8081
    },
    ShopChildSpec = {
        shop_api_server,
        {erf, start_link, [ShopAPIConf]},
        permanent,
        5000,
        worker,
        [erf]
    },
    {ok, {{one_for_one, 5, 10}, [ShopChildSpec]}}.
