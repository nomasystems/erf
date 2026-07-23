%% An <code>erf</code> callback for the versioned products REST API. `price` used to be a
%% plain number (v1); to support multiple currencies, v2 turned it into a `{amount, currency}`
%% object -- a realistic, backwards-incompatible shape change. This module illustrates the
%% three ways a callback function can relate to the versions that share its operationId:
%%   - `list_products/1` is identical in v1 and v2: one function, no mention of `version` at
%%     all.
%%   - `create_product/1` diverges (v1's plain-number `price` vs v2's `{amount, currency}`):
%%     one function, branching inline on the `version` field of the request, so v1 clients
%%     keep sending a plain number forever.
%%   - `discontinue_product/1` only exists in v2's specification, so it is only ever called
%%     for v2 requests: no divergence to handle, because there's nothing to share with v1.
-module(products_callback).

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
    Products = [Product || {_Id, Product} <- ets:tab2list(products)],
    {200, [], Products}.

create_product(#{version := <<"v1">>, body := #{<<"price">> := Price} = Body}) ->
    % v1's `price` is a plain number: normalize it to v2's `{amount, currency}` shape, assuming
    % USD, so the rest of the system only ever deals with one representation of a price.
    store_product(Body#{<<"price">> => #{<<"amount">> => Price, <<"currency">> => <<"USD">>}});
create_product(#{body := Body}) ->
    store_product(Body).

discontinue_product(#{path_parameters := PathParameters}) ->
    Id = proplists:get_value(<<"productId">>, PathParameters),
    ets:delete(products, Id),
    {204, [], undefined}.

%%%-------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-------------------------------------------------------
store_product(Product) ->
    % Hex-encoded, unlike base64, never needs URL-encoding to be used in a path segment.
    Id = binary:encode_hex(crypto:strong_rand_bytes(8)),
    StoredProduct = Product#{<<"id">> => Id},
    ets:insert(products, {Id, StoredProduct}),
    {201, [], StoredProduct}.
