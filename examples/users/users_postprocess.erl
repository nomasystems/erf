%% An <code>erf</code> postprocess middleware for the users REST API.
-module(users_postprocess).

-behaviour(erf_postprocess_middleware).

%%% EXTERNAL EXPORTS
-export([
    postprocess/2
]).

%%%-------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-------------------------------------------------------
% Here we exemplify how information previously inserted on the request context
% can be used to condition the request processing flow.
postprocess(#{method := post, context := #{post_init := PostInitT}} = _Request, Response) ->
    PostEndT = erlang:timestamp(),
    Diff = timer:now_diff(PostEndT, PostInitT),
    io:format("Post time diff : ~p~n", [Diff]),
    Response;
postprocess(_Request, Response) ->
    Response.
