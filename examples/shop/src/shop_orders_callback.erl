%% An <code>erf</code> callback for the orders mount, served from the root.
-module(shop_orders_callback).

%%% EXTERNAL EXPORTS
-export([
    list_orders/1,
    create_order/1,
    get_order/1
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
list_orders(_Request) ->
    {200, [], [Order || {_Id, Order} <- ets:tab2list(orders)]}.

create_order(#{body := Body} = _Request) ->
    Id = base64:encode(crypto:strong_rand_bytes(16), #{mode => urlsafe, padding => false}),
    Order = Body#{<<"id">> => Id},
    ets:insert(orders, {Id, Order}),
    {201, [], Order}.

get_order(#{path_parameters := PathParameters} = _Request) ->
    Id = proplists:get_value(<<"orderId">>, PathParameters),
    case ets:lookup(orders, Id) of
        [] ->
            {404, [], #{<<"message">> => <<"Order ", Id/binary, " not found">>}};
        [{Id, Order}] ->
            {200, [], Order}
    end.
