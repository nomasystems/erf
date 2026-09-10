%% v2's controller for the products REST API: `price` is a `{amount, currency}` object, the
%% same shape `products_store` keeps internally, so no translation is needed here. v2 also
%% adds `discontinue_product/1`, an operation v1 never had.
-module(products_v2_callback).

%%% EXTERNAL EXPORTS
-export([
    list_products/1,
    create_product/1,
    discontinue_product/1
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
list_products(_Request) ->
    {200, [], products_store:list()}.

create_product(#{body := #{<<"name">> := Name, <<"price">> := Price}}) ->
    Product = products_store:create(Name, Price),
    {201, [], Product}.

discontinue_product(#{path_parameters := PathParameters}) ->
    Id = proplists:get_value(<<"productId">>, PathParameters),
    ok = products_store:discontinue(Id),
    {204, [], undefined}.
