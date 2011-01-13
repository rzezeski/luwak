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
    {ok, Deleted} =
        case wrq:get_qs_value("timeout", RD) of
            undefined -> luwak_tree:gc_orphans(C);
            Timeout -> luwak_tree:gc_orphans(C, list_to_integer(Timeout))
        end,
    RD2 = wrq:set_resp_body(list_to_binary(integer_to_list(Deleted)), RD),
    {true, RD2, Ctx}.
