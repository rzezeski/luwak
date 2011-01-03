-module(luwak_wm_gc).
-author('Ryan Zezeski <rzezeski@gmail.com>').

-export([
         init/1,
         allowed_methods/2,
         process_post/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, none}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

process_post(RD, Ctx) ->
    {ok, C} = riak:local_client(),
    luwak_tree:gc(C),
    {true, RD, Ctx}.

