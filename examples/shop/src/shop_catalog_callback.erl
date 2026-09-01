%% An <code>erf</code> callback for the catalog mount, served from `/catalog'.
-module(shop_catalog_callback).

%%% EXTERNAL EXPORTS
-export([
    list_products/1,
    create_product/1,
    get_product/1
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
list_products(_Request) ->
    {200, [], [Product || {_Id, Product} <- ets:tab2list(products)]}.

create_product(#{body := Body} = _Request) ->
    Id = base64:encode(crypto:strong_rand_bytes(16), #{mode => urlsafe, padding => false}),
    Product = Body#{<<"id">> => Id},
    ets:insert(products, {Id, Product}),
    {201, [], Product}.

get_product(#{path_parameters := PathParameters} = _Request) ->
    Id = proplists:get_value(<<"productId">>, PathParameters),
    case ets:lookup(products, Id) of
        [] ->
            {404, [], #{<<"message">> => <<"Product ", Id/binary, " not found">>}};
        [{Id, Product}] ->
            {200, [], Product}
    end.
