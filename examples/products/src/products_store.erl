%% Storage shared by every version's callback module. Prices are always stored in v2's
%% canonical `{amount, currency}` shape; version-specific translation happens in each
%% callback module, not here.
-module(products_store).

%%% EXTERNAL EXPORTS
-export([
    list/0,
    create/2,
    discontinue/1
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
list() ->
    [Product || {_Id, Product} <- ets:tab2list(products)].

create(Name, Price) ->
    % Hex-encoded, unlike base64, never needs URL-encoding to be used in a path segment.
    Id = binary:encode_hex(crypto:strong_rand_bytes(8)),
    Product = #{<<"id">> => Id, <<"name">> => Name, <<"price">> => Price},
    ets:insert(products, {Id, Product}),
    Product.

discontinue(Id) ->
    ets:delete(products, Id),
    ok.
