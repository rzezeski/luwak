-module(luwak_block_tests).

-include_lib("eunit/include/eunit.hrl").

create_test() ->
    test_helper:riak_test(
      fun(Riak) ->
              Data = <<"i am a block">>,
              {ok, Block} = luwak_block:create(Riak, Data),
              Hash = <<"19825ad8650db13cb8f1de37720d3136f354cfd3901bc8863e6b4ac7fdb673e1490937695d6db55a45e577935a9951f52b490f401519486a419354852c4828e9">>,
              ?assertEqual(Hash, luwak_block:name(Block)),
              Val = riak_object:get_value(Block),
              ?assertEqual(Data, proplists:get_value(data,Val)),
              ?assertEqual(block, proplists:get_value(type,Val)),
              ?assertEqual(1, proplists:get_value(refs,Val))
      end).

ref_test() ->
    test_helper:riak_test(
      fun(Riak) ->
              Data = <<"make sure refs is incremented">>,
              {ok, Block} = luwak_block:create(Riak, Data),
              Val = riak_object:get_value(Block),
              ?assertEqual(1, proplists:get_value(refs,Val)),

              {ok, Block2} = luwak_block:create(Riak, Data),
              Val2 = riak_object:get_value(Block2),
              ?assertEqual(2, proplists:get_value(refs,Val2))
      end).
