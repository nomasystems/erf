%% v1's controller for the products REST API: `price` is a plain number here. Products are
%% stored internally in v2's `{amount, currency}` shape (see `products_store`), so this
%% module translates it back and forth -- v1 clients never need to know the internal
%% representation changed.
-module(products_v1_callback).

%%% EXTERNAL EXPORTS
-export([
    list_products/1,
    create_product/1
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
list_products(_Request) ->
    Products = [to_v1_shape(Product) || Product <- products_store:list()],
    {200, [], Products}.

create_product(#{body := #{<<"name">> := Name, <<"price">> := Price}}) ->
    Product = products_store:create(Name, #{<<"amount">> => Price, <<"currency">> => <<"USD">>}),
    {201, [], to_v1_shape(Product)}.

%%%-------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-------------------------------------------------------
to_v1_shape(#{<<"price">> := #{<<"amount">> := Amount}} = Product) ->
    Product#{<<"price">> => Amount}.
