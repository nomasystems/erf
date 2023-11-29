%% An <code>erf</code> postprocess middleware for the users REST API.
-module(users_postprocess).

-behaviour(erf_postprocess_middleware).

-include_lib("kernel/include/logger.erl").

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
    Diff = timer:now_diff(PostInitT, PostEndT),
    logger:info("Post time diff : ~p", [Diff]),
    Response;
postprocess(_Request, Response) ->
    Response.
