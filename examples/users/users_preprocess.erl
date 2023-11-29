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
preprocess(#{method := delete} = Request) ->
    case are_deletes_disabled() of
        true ->
            % For delete operations, if delete is disabled,
            % we skip to the post-process middlewares.
            {stop, {403, [], <<"Delete operations are forbidden right now">>}};
        false ->
            % If not, let the following middlewares and callback module
            % process the request as is.
            Request
    end;
preprocess(#{method := post} = Request) ->
    PostInitT = erlang:timestamp(),
    Context = maps:get_value(context, Request, #{}),
    % We store the current timestamp on the the request context
    % for latter use.
    Request#{context => Context#{post_init => PostInitT}};
preprocess(Request) ->
    Request.
