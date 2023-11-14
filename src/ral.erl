-module(ral).

%% API exports
-export([test/0]).

get(RA_list, Index) ->
    todo.

search(RAL, Index, Acc_skipped) ->
    todo.

build_bin_list(_,1) ->
    pass
build_bin_list(N,Bit_space) ->
    pass

find(Tree, Traversal_list_binary) -> Value;
find(nil, Traversal_list_binary) -> nil;
find({Element, _, _}, []) -> Element;
find({_, Next_l, _}, [0|T]) -> find(Next_l, T);
find({_, Next_l, _}, [1|T]) -> find(Next_r, T).

update(Tree, Index, Value, Acc_skipped) ->
    todo.



%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
RAL1 = [{6, nil, nil}, {2, {2, nil, nil}, {18, nil, nil}}, {4, {2, {5, nil, nil}, {9, nil, nil}}, {2, {2, nil, nil}, {13, nil, nil}}}],
RAL2 = [{1, nil, nil}, {2, {4, nil, nil}, {2, nil, nil}}]

get_test_()->
	[?_assertEqual(nil, get(nil)),%happy path
    ?_assertEqual(13, get(1))
    ?_assertEqual(, get())
	 %nasty thoughts start here
	].

search_test_()->
	[?_assertEqual(nil, search(nil)),%happy path
	 %nasty thoughts start here
	].

build_bin_list_test_()->
	[?_assertEqual(nil, build_bin_list(nil)),%happy path
	 %nasty thoughts start here
	].

find_test_()->
	[?_assertEqual(nil, find(nil)),%happy path
	 %nasty thoughts start here
	].

update_test_()->
	[?_assertEqual(nil, update(nil)),%happy path
	 %nasty thoughts start here
	].

replace_test_()->
	[?_assertEqual(nil, replace(nil)),%happy path
	 %nasty thoughts start here
	].

cons_test_()->
	[?_assertEqual(nil, cons(nil)),%happy path
	 %nasty thoughts start here
	].

link_test_()->
	[?_assertEqual(nil, link(nil)),%happy path
	 %nasty thoughts start here
	].


-endif.