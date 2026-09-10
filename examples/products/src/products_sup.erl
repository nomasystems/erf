%% Supervisor exemplifying erf's API versioning: a single `erf` instance serves both v1 and
%% v2 of the same "products" API, sharing one callback module.
-module(products_sup).

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
    % Products storage
    ets:new(products, [public, named_table]),
    ProductsAPIConf = #{
        spec_path => #{
            <<"v1">> => filename:join(code:priv_dir(products), <<"products_v1.openapi.json">>),
            <<"v2">> => filename:join(code:priv_dir(products), <<"products_v2.openapi.json">>)
        },
        % v1 is also served unprefixed, so pre-existing clients of a single-spec `/products`
        % API are unaffected by the switch to versioning.
        default_version => <<"v1">>,
        callback => #{
            <<"v1">> => products_v1_callback,
            <<"v2">> => products_v2_callback
        },
        port => 8081
    },
    ProductsChildSpec = {
        public_api_server,
        {erf, start_link, [ProductsAPIConf]},
        permanent,
        5000,
        worker,
        [erf]
    },
    {ok, {{one_for_one, 5, 10}, [ProductsChildSpec]}}.
