-module(luwak_block).

-include_lib("luwak.hrl").

-export([create/2,
         data/1,
         name/1]).

create(Riak, Data) ->
    Hash = skerl:hexhash(?HASH_LEN, Data),
    Block =
        case Riak:get(?N_BUCKET, Hash) of
            {ok, B} ->
                Value = riak_object:get_value(B),
                Refs = proplists:get_value(refs,Value),
                Value2 = lists:keyreplace(refs,1,Value,{refs,Refs+1}),
                riak_object:update_value(B, Value2);
            {error, notfound} ->
                Value = [
                         {data, Data},
                         {created, now()},
                         {type, block},
                         {refs,1}
                        ],
                riak_object:new(?N_BUCKET, Hash, Value)
        end,
    Riak:put(Block, 2, 2, ?TIMEOUT_DEFAULT, [{returnbody, true}]).

data(Val) when is_list(Val) ->
    proplists:get_value(data, Val);
data(Object) ->
    proplists:get_value(data, riak_object:get_value(Object)).

name(Object) ->
    riak_object:key(Object).
