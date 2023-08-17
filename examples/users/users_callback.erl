%% An <code>erf</code> callback for the users REST API.
-module(users_callback).

%%% EXTERNAL EXPORTS
-export([
    create_user/4,
    get_user/4,
    delete_user/4
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
create_user(_PathParameters, _QueryParameters, _Headers, Body) ->
    Id = base64:encode(crypto:strong_rand_bytes(16)),
    ets:insert(users, {Id, Body#{<<"id">> => Id}}),
    {201, [], Body#{<<"id">> => Id}}.

get_user(PathParameters, _QueryParameters, _Headers, _Body) ->
    Id = proplists:get_value(<<"userId">>, PathParameters),
    case ets:lookup(users, Id) of
        [] ->
            {404, [], #{
                <<"message">> =>
                    <<"User ", Id/binary, " not found">>
            }};
        [{Id, User}] ->
            {200, [], User}
    end.

delete_user(PathParameters, _QueryParameters, _Headers, _Body) ->
    Id = proplists:get_value(<<"userId">>, PathParameters),
    case ets:lookup(users, Id) of
        [] ->
            {404, [], #{
                <<"message">> =>
                    <<"User ", Id/binary, " not found">>
            }};
        [_User] ->
            ets:delete(users, Id),
            {204, [], #{<<"id">> => Id}}
    end.
