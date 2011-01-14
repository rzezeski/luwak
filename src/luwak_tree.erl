-module(luwak_tree).

-export([update/4,
         get/2,
         block_at/3,
         visualize_tree/2,
         get_range/6,
         get_range/7,
         truncate/7,
         truncate/1,
         get_roots/1,
         get_nodes/4,
         gc/1,
         gc_orphans/1,
         gc_orphans/2]).

-include_lib("luwak.hrl").

%%=======================================
%% Public API
%%=======================================

update(Riak, File, StartingPos, Blocks) ->
    Order = luwak_file:get_property(File, tree_order),
    BlockSize = luwak_file:get_property(File, block_size),
    if
        StartingPos rem BlockSize =/= 0 ->
            throw({error, ?fmt("StartingPos (~p) must be a multiple of blocksize", [StartingPos])});
        true ->
            ok
    end,
    case luwak_file:get_property(File, root) of
        %% there is no root, therefore we create one
        undefined -> 
            {ok, RootObj} = create_tree(Riak, Order, Blocks),
            RootName = riak_object:key(RootObj),
            luwak_file:update_root(Riak, File, RootName);
        RootName ->
            {ok, Root} = get(Riak, RootName),
            ?debugMsg("blocks~n"),
            ?debugMsg("children~n"),
            {ok, NewRoot} = subtree_update(Riak, File, Order, StartingPos, 0, 
                                           Root, Blocks),
            NewRootName = riak_object:key(NewRoot),
            luwak_file:update_root(Riak, File, NewRootName)
    end.

get_range(Riak, Parent, BlockSize, TreeStart, Start, End) ->
    Fun = fun({Name,Length}, _) when Length =< BlockSize ->
                  ?debugFmt("A foldrflatmap({~p,~p}, _)~n", [Name, Length]),
                  {[{Name,BlockSize}], 0};
             ({Name,NodeLength}, AccLength) ->
                  ?debugFmt("B foldrflatmap({~p,~p}, ~p)~n",
                            [Name, NodeLength, AccLength]),
                  {ok, Node} = get(Riak, Name),
                  Blocks = get_range(Riak, Node, BlockSize, AccLength,
                                     Start, End),
                  {Blocks, AccLength+NodeLength}
          end,
    get_range(Riak, Fun, Parent, BlockSize, TreeStart, Start, End).

get_range(_, _, _, _, _, _, 0) ->
    [];
get_range(_Riak, _Fun, _Parent = #n{children=[]}, _BlockSize, _TreeStart,
          _Start, _End) ->
    ?debugMsg("D get_range(_, _, _, _, _, _)~n"),
    [];
%% children are individual blocks
%% we can do this because trees are guaranteed to be full
get_range(_Riak, Fun, _Parent = #n{children=[{_,BlockSize}|_]=Children},
          BlockSize, TreeStart, Start, End) ->
    ?debugFmt("A get_range(Riak, ~p, ~p, ~p, ~p, ~p)~n",
              [_Parent, BlockSize, TreeStart, Start, End]),
    {Nodes, Length} = read_split(Children, TreeStart, Start, End),
    luwak_tree_utils:foldrflatmap(Fun, Nodes, Length);
get_range(_Riak, Fun, _Parent = #n{children=Children}, _BlockSize, TreeStart,
          Start, End) ->
    ?debugFmt("B get_range(Riak, ~p, ~p, ~p, ~p, ~p)~n",
              [_Parent, _BlockSize, TreeStart, Start, End]),
    {Nodes, Length} = read_split(Children, TreeStart, Start, End),
    luwak_tree_utils:foldrflatmap(Fun, Nodes, Length).

truncate(_Riak, _File, _Start, undefined, _Order, _NodeOffset, _BlockSize) ->
    ?debugFmt("A truncate(Riak, File, ~p, undefined, ~p, ~p, ~p)~n",
              [_Start, _Order, _NodeOffset, _BlockSize]),
    {ok, {undefined,0}};
truncate(Riak, File, Start, _Parent=#n{children=Children},
         Order, NodeOffset, BlockSize) ->
    ?debugFmt("B truncate(Riak, File, ~p, ~p, ~p, ~p, ~p)~n",
              [Start,_Parent,Order,NodeOffset,BlockSize]),
    {Keep, {Recurse,_RecLength}, _} = which_child(Children, NodeOffset,
                                                  Start, []),
    KeepLength = luwak_tree_utils:blocklist_length(Keep),
    {ok, SubNode} = get(Riak, Recurse),
    {ok, NN} = truncate(Riak, File, Start, SubNode, Order,
                        NodeOffset+KeepLength, BlockSize),
    {ok, NewNode} = create_tree(Riak, Order, Keep ++ [NN]),
    NewNodeVal = riak_object:get_value(NewNode),
    {ok, {riak_object:key(NewNode),
          luwak_tree_utils:blocklist_length(NewNodeVal#n.children)}};
truncate(Riak, _File, Start, Block, _Order, NodeOffset, _BlockSize) ->
    ?debugFmt("C truncate(Riak, File, ~p, ~p, ~p, ~p, ~p)~n",
              [Start, Block, _Order, NodeOffset, _BlockSize]),
    Data = luwak_block:data(Block),
    ByteOffset = Start - NodeOffset,
    <<Retain:ByteOffset/binary, _/binary>> = Data,
    {ok, NewBlock} = luwak_block:create(Riak, Retain),
    {ok, {riak_object:key(NewBlock), ByteOffset}}.

read_split(Children, TreeStart, Start, End) when Start < 0 ->
    read_split(Children, TreeStart, 0, End);
read_split(Children, TreeStart, Start, End) ->
    ?debugFmt("read_split(~p, ~p, ~p, ~p)~n",
              [Children, TreeStart, Start, End]),
    InsidePos = Start - TreeStart,
    InsideEnd = End - TreeStart,
    {Head,Tail} = luwak_tree_utils:split_at_length(Children, InsidePos),
    {Middle,_} = luwak_tree_utils:split_at_length_left_bias(Tail, InsideEnd),
    ?debugFmt("middle ~p~n", [Middle]),
    {Middle, luwak_tree_utils:blocklist_length(Head)+TreeStart}.

get(Riak, Name) when is_binary(Name) ->
    {ok, Obj} = Riak:get(?N_BUCKET, Name, 2),
    {ok, riak_object:get_value(Obj)}.

visualize_tree(Riak, RootName) ->
    {ok, Node} = get(Riak, RootName),
    [<<"digraph Luwak_Tree {\n">>,
     <<"# page = \"8.2677165,11.692913\" ;\n">>,
     <<"ratio = \"auto\" ;\n">>,
     <<"mincross = 2.0 ;\n">>,
     <<"label = \"Luwak Tree\" ;\n">>,
     visualize_tree(Riak, RootName, Node),
     <<"}">>].

visualize_tree(Riak, RootName = <<Prefix:8/binary, _/binary>>,
               #n{children=Children}) ->
    io_lib:format("\"~s\" [shape=circle,label=\"~s\","
                  "regular=1,style=filled,fillcolor=white ] ;~n",
                  [RootName,Prefix]) ++
        lists:map(fun({ChildName,_}) ->
                          {ok, Child} = get(Riak, ChildName),
                          visualize_tree(Riak, ChildName, Child)
                  end, Children) ++
        lists:map(fun({ChildName,Length}) ->
                          io_lib:format("\"~s\" -> \"~s\""
                                        " [dir=none,weight=1,label=\"~p\"] ;~n",
                                        [RootName,ChildName,Length])
                  end, Children);
visualize_tree(_Riak, DataName = <<Prefix:8/binary, _/binary>>, DataNode) ->
    Data = luwak_block:data(DataNode),
    PrefixData = if
                     byte_size(Data) > 8 ->
                         <<P:8/binary, _/binary>> = Data,
                         mochihex:to_hex(P);
                     true ->
                         Data
                 end,
    io_lib:format("\"~s\" [shape=record,label=\"~s | ~s\","
                  "regular=1,style=filled,fillcolor=gray ] ;~n",
                  [DataName,Prefix,PrefixData]).

create_tree(Riak, Order, Children) when is_list(Children) ->
    ?debugFmt("create_tree(Riak, ~p, ~p)~n", [Order, Children]),
    if
        length(Children) > Order ->
            Written = list_into_nodes(Riak, Children, Order, 0),
            create_node(Riak, Written);
        true ->
            create_node(Riak, Children)
    end.

%% updating any node happens in up to 5 parts
%% depending on the coverage of the write list
subtree_update(Riak, File, Order, InsertPos, TreePos, Parent = #n{}, Blocks) ->
    ?debugFmt("subtree_update(Riak, File, ~p, ~p, ~p, ~p, ~p)~n",
              [Order, InsertPos, TreePos, Parent, truncate(Blocks)]),
    {NodeSplit, BlockSplit} = luwak_tree_utils:five_way_split(TreePos, 
                                                              Parent#n.children,
                                                              InsertPos,
                                                              Blocks),
    ?debugFmt("NodeSplit ~p BlockSplit ~p~n", [NodeSplit, BlockSplit]),
    MidHeadStart = luwak_tree_utils:blocklist_length(NodeSplit#split.head) +
        TreePos,
    ?debugMsg("midhead~n"),
    MidHeadReplacement =
        lists:map(
          fun({Name,_Length}) ->
                  {ok, ChildNode} = get(Riak, Name),
                  {ok, ReplacementChild} = subtree_update(Riak, File, Order, 
                                                      InsertPos,
                                                      MidHeadStart, 
                                                      ChildNode,
                                                      BlockSplit#split.midhead),
                  V = riak_object:get_value(ReplacementChild),
                  {riak_object:key(ReplacementChild),
                   luwak_tree_utils:blocklist_length(V#n.children)}
          end,
          NodeSplit#split.midhead),
    MiddleInsertStart = luwak_tree_utils:blocklist_length(
                                  BlockSplit#split.midhead) + MidHeadStart,
    ?debugMsg("middle~n"),
    MiddleReplacement = list_into_nodes(Riak, BlockSplit#split.middle, Order,
                                        MiddleInsertStart),
    MidTailStart = luwak_tree_utils:blocklist_length(BlockSplit#split.middle) +
        MiddleInsertStart,
    ?debugMsg("midtail~n"),
    MidTailReplacement =
        lists:map(
          fun({Name,_Length}) ->
                  {ok, ChildNode} = get(Riak, Name),
                  {ok, ReplacementChild} = subtree_update(Riak, File, Order,
                                                      MidTailStart,
                                                      MidTailStart,
                                                      ChildNode,
                                                      BlockSplit#split.midtail),
                  V = riak_object:get_value(ReplacementChild),
                  {riak_object:key(ReplacementChild),
                   luwak_tree_utils:blocklist_length(V#n.children)}
          end,
          NodeSplit#split.midtail),
    ?debugMsg("end~n"),
    create_tree(Riak, Order, NodeSplit#split.head ++ 
                    MidHeadReplacement ++ 
                    MiddleReplacement ++ 
                    MidTailReplacement ++
                    NodeSplit#split.tail).

list_into_nodes(Riak, Children, Order, StartingPos) ->
    ?debugFmt("list_into_nodes(Riak, ~p, ~p, ~p)~n",
              [Children, Order, StartingPos]),
    Written = map_sublist(
                fun([{SubNode,Length}]) ->
                        {SubNode,Length};
                   (Sublist) ->
                        Length = luwak_tree_utils:blocklist_length(Sublist),
                        {ok, Obj} = create_node(Riak, Sublist),
                        {riak_object:key(Obj), Length}
                end,
                Order,
                Children),
    if
        length(Written) > Order ->
            list_into_nodes(Riak, Written, Order, StartingPos);
        true ->
            Written
    end.

%% @spec block_at(Riak::riak(), File::luwak_file(), Pos::int()) ->
%%          {ok, BlockObj} | {error, Reason}
block_at(Riak, File, Pos) ->
    case luwak_file:get_property(File, root) of
        undefined -> {error, notfound};
        RootName -> block_at_retr(Riak, RootName, 0, Pos)
    end.

block_at_retr(Riak, NodeName, NodeOffset, Pos) ->
    case Riak:get(?N_BUCKET, NodeName, 2) of
        {ok, NodeObj} ->
            Type = luwak_file:get_property(NodeObj, type),
            Links = luwak_file:get_property(NodeObj, links),
            block_at_node(Riak, NodeObj, Type, Links, NodeOffset, Pos);
        Err ->
            Err
    end.

block_at_node(Riak, _NodeObj, node, Links, NodeOffset, Pos) ->
    case which_child(Links, NodeOffset, Pos, []) of
        {Head, {ChildName,_}, _} ->
            block_at_retr(Riak,
                          ChildName,
                          NodeOffset+luwak_tree_utils:blocklist_length(Head),
                          Pos);
        {_, undefined, _} ->
            {ok, undefined}
    end;
block_at_node(_Riak, NodeObj, block, _, _NodeOffset, _) ->
    {ok, NodeObj}.

which_child([E={_ChildName,Length}], NodeOffset, Pos, Acc)
  when Pos >= Length+NodeOffset ->
    ?debugFmt("A which_child(~p, ~p, ~p)~n", [E, NodeOffset, Pos]),
    {lists:reverse([E|Acc]), undefined, []};
which_child([E], _NodeOffset, _Pos, Acc) ->
    ?debugFmt("B which_child(~p, ~p, ~p)~n", [E, _NodeOffset, _Pos]),
    {lists:reverse(Acc), E, []};
which_child([E={_ChildName,Length}|Tail], NodeOffset, Pos, Acc)
  when Pos >= NodeOffset + Length ->
    ?debugFmt("C which_child(~p, ~p, ~p)~n",
              [[{_ChildName,Length}|Tail], NodeOffset, Pos]),
    which_child(Tail, NodeOffset+Length, Pos, [E|Acc]);
which_child([{ChildName,Length}|Tail], NodeOffset, Pos, Acc)
  when Pos < NodeOffset + Length ->
    ?debugFmt("D which_child(~p, ~p, ~p)~n",
              [[{ChildName,Length}|Tail], NodeOffset, Pos]),
    {lists:reverse(Acc), {ChildName,Length}, Tail}.

map_sublist(Fun, N, List) ->
    map_sublist_1(Fun, N, List, [], []).

map_sublist_1(_, _, [], [], Acc) ->
    lists:reverse(Acc);
map_sublist_1(_, N, [], Sublist, []) when length(Sublist) < N ->
    lists:reverse(Sublist);
map_sublist_1(Fun, _N, [], Sublist, Acc) ->
    lists:reverse([Fun(lists:reverse(Sublist))|Acc]);
map_sublist_1(Fun, N, List, Sublist, Acc) when length(Sublist) >= N ->
    Result = Fun(lists:reverse(Sublist)),
    map_sublist_1(Fun, N, List, [], [Result|Acc]);
map_sublist_1(Fun, N, [E|List], Sublist, Acc) ->
    map_sublist_1(Fun, N, List, [E|Sublist], Acc).

create_node(Riak, Children) ->
    ?debugFmt("create_node(Riak, ~p)~n", [Children]),
    N = #n{created=now(),children=Children},
    Name = skerl:hexhash(?HASH_LEN, term_to_binary(Children)),
    Obj = riak_object:new(?N_BUCKET, Name, N),
    Riak:put(Obj, 2, 2, ?TIMEOUT_DEFAULT, [{returnbody, true}]).

truncate(List) when is_list(List) ->
    lists:map(fun({Data,Length}) -> {truncate(Data),Length} end, List);
truncate(<<Prefix:8/binary, _/binary>>) -> Prefix.

%% Garbage Collection
gc(Riak) ->
    {I1, R1} = get_roots(Riak),
    {Nodes, MaxLevel} = get_nodes(Riak, I1, R1, 2),
    Levels = lists:seq(1, MaxLevel),
    GL = group_levels(Nodes, Levels),
    lists:foreach(fun(I) ->
                          In = dict:fetch(I, GL),
                          Riak:mapred(In, [{map, {qfun, fun del/3}, none, false}])
                  end,
                  lists:reverse(Levels)),
    Riak:mapred_bucket(?D_BUCKET, [{map, {qfun, fun del/3}, none, false}]).


del({error, notfound}, _, _) ->
    [];
del(Node, undefined, none) ->
    Bucket = riak_object:bucket(Node),
    {ok, Riak} = riak:local_client(),
    Riak:delete(Bucket, riak_object:key(Node), 2),
    [].

get_roots(Riak) ->
    Level = 1,                                  % tree level
    {ok, [Deleted]} = Riak:mapred_bucket(?D_BUCKET,
                                       [{map, {qfun, fun root_map/3}, {0, Level}, false},
                                        {reduce, {qfun, fun tally/2}, none, true}]),
    {ok, [Exist]} = Riak:mapred_bucket(?O_BUCKET,
                                    [{map, {qfun, fun root_map/3}, {1, Level}, false},
                                     {reduce, {qfun, fun tally/2}, none, true}]),
    Roots = dict:merge(fun add/3, Deleted, Exist),
    Inputs = dict_to_inputs(Roots),
    {Inputs, Roots}.

get_nodes(_Riak, [], Nodes, Level) ->
    {Nodes, Level};
get_nodes(Riak, Inputs, Nodes, Level) ->
    Mark =
        fun({error, notfound}, _, _) ->
                [];
           (Node, Num, none) ->
                Tally = if Num > 0 -> 1; true -> 0 end,
                Value = riak_object:get_value(Node),
                case Value of
                    #n{children = Children} ->
                        [dict:from_list([{Name, {Tally, Level}} || {Name, _} <- Children])];
                    _Block ->
                        []
                end
        end,
    {ok, [Tallies]} = Riak:mapred(Inputs,
                                  [{map, {qfun, Mark}, none, false},
                                   {reduce, {qfun, fun tally/2}, none, true}]),
    case dict:size(Tallies) of
        0 ->
            {Nodes, Level - 1};
        _ ->
            Inputs2 = dict_to_inputs(Tallies),
            Nodes2 = dict:merge(fun add/3, Nodes, Tallies),
            get_nodes(Riak, Inputs2, Nodes2, Level + 1)
    end.

dict_to_inputs(D) ->
    [{{?N_BUCKET, Name}, Refs} || {Name, {Refs, _}} <- dict:to_list(D)].

group_levels(Nodes, Levels) ->
    GL = lists:foldl(fun(I, Acc) -> dict:store(I, [], Acc) end,
                     dict:new(),
                     Levels),
    F = fun(Name, {0, Level}, Acc) ->
                dict:append(Level, {?N_BUCKET, Name}, Acc);
           (_, _, Acc) ->
                Acc
        end,
    dict:fold(F, GL, Nodes).

counts(I, Level) ->
    fun(undefined, Acc) -> Acc;
       (N, Acc)         -> [{N, {I, Level}}|Acc]
    end.

add(_, {C1, L1}, {C2, L2}) ->
    L3 = if
             L1 < L2 ->
                 L1;
             true ->
                 L2
         end,
    {C1 + C2, L3}.

root_map({error, notfound}, _, _) ->
    [];
root_map(Obj, undefined, {Ref, Level}) ->
    case riak_object:get_value(Obj) of
        deleted_node ->
            [dict:from_list([{riak_object:key(Obj), {Ref, Level}}])];
        FileValue ->
            Root = proplists:get_value(root, FileValue),
            Ancestors = proplists:get_value(ancestors, FileValue),
            [dict:from_list(lists:foldl(counts(Ref, Level), [], [Root|Ancestors]))]
    end.

tally(Tallies, none) ->
    Merge = fun(T, Acc) ->
                    dict:merge(fun add/3, T, Acc)
            end,
    [lists:foldl(Merge, dict:new(), Tallies)].

%% an orphan is a node (at any level of the tree) that has nothing
%% referencing it, this call will remove things from the top-down.
gc_orphans(Riak) ->
    gc_orphans(Riak, ?TIMEOUT_DEFAULT).

gc_orphans(Riak, Timeout) ->
    {ok, Roots} = Riak:mapred_bucket(?O_BUCKET,
                                     [{map, {qfun, fun root_map2/3}, none, true}],
                                     Timeout),
    %% Don't GC anything newer than now or else dangling pointers may
    %% result
    Now = erlang:now(),
    {ok, Nodes} = Riak:mapred_bucket(?N_BUCKET,
                                     [{map, {qfun, fun tally/3}, Now, false},
                                      {reduce, {qfun, fun sum/2}, none, true}],
                                     Timeout),
    Delete = lists:filter(fun zero/1, sum(Roots ++ Nodes, none)),
    lists:foreach(fun del/1, Delete),
    {ok, length(Delete)}.

root_map2({error, notfound}, _, _) ->
    [];
root_map2(Obj, undefined, none) ->
    V = riak_object:get_value(Obj),
    Root = proplists:get_value(root, V),
    Ancestors = lists:filter(fun defined/1, proplists:get_value(ancestors, V)),
    [{Name, 1} || Name <- [Root|Ancestors]].

defined(undefined) ->
    false;
defined(_) ->
    true.

tally({error, notfound}, _, _) ->
    [];
tally(Obj, undefined, Now) ->
    Value = riak_object:get_value(Obj),
    Name = riak_object:key(Obj),
    case Value of
        #n{created = Created, children = Children} ->
            if Created < Now ->
                    [{Name, 0} | [{ChildName, 1} || {ChildName, _} <- Children]];
               true ->
                    []
            end;
        [_, {created, Created}, _] ->
            if Created < Now ->
                    [{Name, 0}];
               true ->
                    []
            end
    end.

sum(Tallies, none) ->
    F = fun({Name, T}, D) ->
                dict:update_counter(Name, T, D)
        end,
    dict:to_list(lists:foldl(F, dict:new(), Tallies)).

zero({_, 0}) ->
    true;
zero({_, _}) ->
    false.

del({Name, 0}) ->
    {ok, Riak} = riak:local_client(),
    Riak:delete(?N_BUCKET, Name, 1).
