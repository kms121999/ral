-module(ral).

%% API exports
-export([build_bin_list/2]).

% This function ultimatly gets a value from the random access list. It has a
% few helper functions. They are search, build_bin_list, and find.
% Paramiters: R_A_list (ra list), Index (the value we are searching for)
get([], _Index) -> nil;
get([{Element, nil, nil}|_T], 0) ->
	Element;
get(R_A_list, Index) ->
  	{Skipped_indicies, Tree} = search(R_A_list, Index, 0),
	if
		Tree =:= nil ->
			nil;
		true ->
			{Leaf_count, _, _} = Tree,
			find(Tree, build_bin_list(Index - Skipped_indicies, Leaf_count))
	end.

% search does stuff
search([], Index, _) -> {fail, nil};
search([H|T], 0, _) -> {0,H};
search([{_, nil, nil}|T], Index, Accum) -> {};
search([{Leaf_count, Next_l, Next_r}|T], Index, Accum) when Leaf_count < Index - Accum ->
	search(T, Index, Accum + Leaf_count);
search([H|_T], Index, Accum) ->
	{Accum, H}.



build_bin_list(_,1) ->
    [];
build_bin_list(_,1.0) ->
    [];
build_bin_list(N,Bit_space) ->
    Lesser_power = Bit_space / 2,
		case N >= Lesser_power of
			true ->
				[1] ++ build_bin_list(N - Lesser_power, Lesser_power);
			false ->
				[0] ++ build_bin_list(N, Lesser_power)
		end.

% This finds the a node in the random access list
% Paramiters: Element (Your search value), Next_l (left node), Next_r (right node).
find(nil, Traversal_list_binary) -> nil;
find({Element, _, _}, []) -> Element;
find({_, Next_l, _}, [0|T]) -> find(Next_l, T); 
find({_, _, Next_r}, [1|T]) -> find(Next_r, T).


update([], _, _, _) -> fail;
update([{_, nil, nil}], 0, Value, _) -> [{Value, nil, nil}];
update([{Leaf_value, nil, nil}|T], Index, Value, Accum) ->
	[{Leaf_value, nil, nil}] ++ update(T, Index, Value, Accum+1);
update([{Leaf_count, Next_l, Next_r}|T], Index, Value, Accum) when Leaf_count < Index - Accum ->
    [{Leaf_count, Next_l, Next_r}] ++ update(T, Index, Value, Accum + Leaf_count);
update([H|T], Index, Value, Accum)->
	{Leaf_count, _Next_l, _Next_r} = H,
    [replace(H, (build_bin_list(Index - Accum, Leaf_count)), Value)] ++ T.


replace(nil, _Traversal_list_binary, _Value) -> 
	fail;
replace({_Old_value, nil, nil}, [], Value) -> 
	{Value,nil,nil};
replace({Weight, Next_l, Next_r}, [0|T], Value) ->
	{Weight, replace(Next_l, T, Value), Next_r};
replace({Weight, Next_l, Next_r}, [1|T], Value) ->
	{Weight, Next_l, replace(Next_r, T, Value)}.


% This function inserts a node into the random access tree.
% Paramiters: Leaf, List
cons(nil, RAL)->
	fail;
cons(Leaf, nil)->
	fail;
cons({_, Next_l, Next_r}, _) when not is_atom(Next_l) or not is_atom(Next_r) ->
	fail;
cons(Leaf, []) ->
	[Leaf];
cons(Leaf, [{Value, nil, nil}=H|T]) ->
	link({2, Leaf, H}, T);
cons(Leaf, RAL) ->
	Leaf ++ RAL.

% this is link
link(Tree, []) -> [Tree];
link(Tree, [{Value, nil, nil}=H|T]) ->
	link({2, Tree, H}, T);
link(Tree, [H|T]) ->
	{Weight, _, _} = Tree,
	{H_weight, _, _} = H,
	link({Weight + H_weight, Tree, H}, T).


%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
% Insertion order 18, 6, 1, 5, 7, 3, 42, 13
% RAL0 = [],
% RAL1 = [{18, nil, nil}],
% RAL2 = [{2, {6, nil, nil}, {18, nil, nil}}],
% RAL3 = [{1, nil, nil}, {2, {6, nil, nil}, {18, nil, nil}}],
% RAL4 = [{4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
% RAL5 = [{7, nil, nil}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
% RAL6 = [{2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
% RAL7 = [{42, nil, nil}, {2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
% RAL8 = [{8, {4, {2, {13, nil, nil}, {42, nil, nil}}, {2, {3, nil, nil}, {7, nil, nil}}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}}],


get_test_()->
	RAL0 = [],
	RAL1 = [{18, nil, nil}],
	RAL2 = [{2, {6, nil, nil}, {18, nil, nil}}],
	RAL3 = [{1, nil, nil}, {2, {6, nil, nil}, {18, nil, nil}}],
	RAL4 = [{4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL5 = [{7, nil, nil}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL6 = [{2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL7 = [{42, nil, nil}, {2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL8 = [{8, {4, {2, {13, nil, nil}, {42, nil, nil}}, {2, {3, nil, nil}, {7, nil, nil}}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}}],
	[?_assertEqual(nil, get(RAL0, 7)),%happy path
    ?_assertEqual(18, get(RAL1, 0)),
    ?_assertEqual(18, get(RAL2, 1)),
    ?_assertEqual(6, get(RAL3, 1)),
    ?_assertEqual(1, get(RAL6, 3))
	 %nasty thoughts start here
	].

% search_test_()->
% 	[?_assertEqual(nil, search(nil)),%happy path
% 	 %nasty thoughts start here
% 	].

build_bin_list_test_()->
	[?_assertEqual([0], build_bin_list(0, 2)),%happy path
	?_assertEqual([1], build_bin_list(1, 2)),%happy path
	?_assertEqual([0, 0], build_bin_list(0, 4)),%happy path
	?_assertEqual([1, 0], build_bin_list(2, 4)),%happy path
	?_assertEqual([0, 1, 1, 0], build_bin_list(6, 16))%happy path
	 %nasty thoughts start here
	].

% find_test_()->
% 	[?_assertEqual(nil, find(nil)),%happy path
% 	 %nasty thoughts start here
% 	].

update_test_()->
	RAL0 = [],
	RAL1 = [{18, nil, nil}],
	RAL2 = [{2, {6, nil, nil}, {18, nil, nil}}],
	RAL3 = [{1, nil, nil}, {2, {6, nil, nil}, {18, nil, nil}}],
	RAL4 = [{4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL5 = [{7, nil, nil}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL6 = [{2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL7 = [{42, nil, nil}, {2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL8 = [{8, {4, {2, {13, nil, nil}, {42, nil, nil}}, {2, {3, nil, nil}, {7, nil, nil}}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}}],
	[?_assertEqual([{7, nil, nil}], update(RAL1, 0, 7, 0)),%happy path
	?_assertEqual([{{6, nil, nil}, {7, nil, nil}}], update(RAL2, 0, 7, 0))%happy path
	 %nasty thoughts start here
	].

% replace_test_()->
% 	[?_assertEqual(nil, replace(nil)),%happy path
% 	 %nasty thoughts start here
% 	].

cons_test_()->
	RAL0 = [],
	RAL1 = [{18, nil, nil}],
	RAL2 = [{2, {6, nil, nil}, {18, nil, nil}}],
	RAL3 = [{1, nil, nil}, {2, {6, nil, nil}, {18, nil, nil}}],
	RAL4 = [{4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL5 = [{7, nil, nil}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL6 = [{2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL7 = [{42, nil, nil}, {2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL8 = [{8, {4, {2, {13, nil, nil}, {42, nil, nil}}, {2, {3, nil, nil}, {7, nil, nil}}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}}],
	[?_assertEqual(RAL1, cons({18, nil, nil}, RAL0)),%happy path
	 ?_assertEqual(RAL2, cons({6, nil, nil}, RAL1)),%happy path
	 ?_assertEqual(RAL3, cons({1, nil, nil}, RAL2)),%happy path
	 ?_assertEqual(RAL4, cons({5, nil, nil}, RAL3)),%happy path
	 ?_assertEqual(RAL5, cons({7, nil, nil}, RAL4)),%happy path
	 ?_assertEqual(RAL6, cons({3, nil, nil}, RAL5)),%happy path
	 ?_assertEqual(RAL7, cons({42, nil, nil}, RAL6)),%happy path
	 ?_assertEqual(RAL8, cons({13, nil, nil}, RAL7)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual(nil, cons(nil, nil)),
	 ?_assertEqual(RAL1, cons(nil, RAL1)),
	 ?_assertEqual(nil, cons({18, nil, nil}, nil))
	].

link_test_()->
	RAL0 = [],
	RAL1 = [{18, nil, nil}],
	RAL2 = [{2, {6, nil, nil}, {18, nil, nil}}],
	RAL3 = [{1, nil, nil}, {2, {6, nil, nil}, {18, nil, nil}}],
	RAL4 = [{4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL5 = [{7, nil, nil}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL6 = [{2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL7 = [{42, nil, nil}, {2, {3, nil, nil}, {7, nil, nil}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}],
	RAL8 = [{8, {4, {2, {13, nil, nil}, {42, nil, nil}}, {2, {3, nil, nil}, {7, nil, nil}}}, {4, {2, {5, nil, nil}, {1, nil, nil}}, {2, {6, nil, nil}, {18, nil, nil}}}}],
	[?_assertEqual([{4, {2, {6, nil, nil}, {18, nil, nil}}, {2, {1, nil, nil}, {3, nil, nil}}}], link(RAL2, [{2, {1, nil, nil}, {3, nil, nil}}])),%happy path
	?_assertEqual(RAL2, link(RAL2, []))%happy path
	 %nasty thoughts start here
	].


-endif.