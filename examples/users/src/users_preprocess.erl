%% An <code>erf</code> preprocess middleware for the users REST API.
-module(users_preprocess).

-behaviour(erf_preprocess_middleware).

%%% EXTERNAL EXPORTS
-export([
    preprocess/1
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
preprocess(#{headers := Headers} = Request) ->
    Authorization = proplists:get_value(<<"x-api-key">>, Headers, undefined),
    case is_authorized(Authorization) of
        false ->
            % For delete operations, if delete is disabled,
            % we skip to the post-process middlewares.
            {stop, {403, [], <<"Missing valid basic authorization header">>}};
        true ->
            PostInitT = erlang:timestamp(),
            Context = maps:get(context, Request, #{}),
            % We store the current timestamp on the the request context
            % for latter use.
            Request#{context => Context#{post_init => PostInitT}}
    end.

%%%-------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-------------------------------------------------------
is_authorized(undefined) ->
    false;
is_authorized(<<"api-key">>) ->
    true;
is_authorized(_) ->
    false.
