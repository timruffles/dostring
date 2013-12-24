-module(pairs).

-include_lib("eunit/include/eunit.hrl").

pairs (Xs) ->
  case Xs of
    [] -> [];
    [X] -> [{X}];
    [X,Y] -> [{X,Y}];
    [X,Y|Rest] -> [{X,Y}|pairs(Rest)]
  end.

tail_rec_pairs (Xs) ->
  tail_rec_pairs(Xs,[]).
tail_rec_pairs ([],Acc) ->
  lists:reverse(Acc);
tail_rec_pairs ([H1,H2|T],Acc) ->
  tail_rec_pairs(T,[{H1,H2}|Acc]);
tail_rec_pairs ([H|T],Acc) ->
  tail_rec_pairs(T,[{H}|Acc]).

pairs_test () ->
  % correctness
  [] = pairs([]),
  [{1}] = pairs([1]),
  [{1,2},{3,4}] = pairs([1,2,3,4]),
  [{1,2}] = pairs([1,2]),
  [{1,2},{3,4},{5}] = pairs([1,2,3,4,5]),

  [] = tail_rec_pairs([]),
  [{1}] = tail_rec_pairs([1]),
  [{1,2},{3,4}] = tail_rec_pairs([1,2,3,4]),
  [{1,2}] = tail_rec_pairs([1,2]),
  [{1,2},{3,4},{5}] = tail_rec_pairs([1,2,3,4,5]),

  {T1, _} = timer:tc(fun pairs/1, [lists:seq(1,10000)]),
  {T2, _} = timer:tc(fun tail_rec_pairs/1, [lists:seq(1,10000)]),
  ?debugFmt("~nbody req: ~p~ntail req: ~p~n",[T1,T2]).
  % [{1,2} | _Rest] = pairs(lists:seq(1,50000000000)).
