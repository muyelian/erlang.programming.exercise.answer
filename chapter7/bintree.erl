-module(bintree).

-export([new/0, delete/1]).
-export([sum/1, max/1, min/1, is_ordered/1, insert/2]).

-record(bt, {value, left=[], right=[]}).

new() ->
  [].

delete(_) ->
  ok.

sum([]) ->
  0;
sum(#bt{value=V, left=L, right=R}) ->
  V + sum(L) + sum(R).

max([]) ->
  [];
max(#bt{value=V, left=L, right=R}) ->
  lists:max(lists:filter(fun erlang:is_number/1, [V, max(L), max(R)])).

min([]) ->
  [];
min(#bt{value=V, left=L, right=R}) ->
  lists:min(lists:filter(fun erlang:is_number/1, [V, min(L), min(R)])).

is_ordered([]) ->
  true;
is_ordered(#bt{value=V, left=L, right=R}) ->
  ((L == []) orelse (is_ordered(L) andalso max(L) =< V))
  andalso
  ((R == []) orelse (is_ordered(R) andalso V =< min(R))).

insert([], NewVal) ->
  #bt{value=NewVal};
insert(#bt{value=V, left=L}=T, NewVal) when NewVal < V ->
  T#bt{left=insert(L, NewVal)};
insert(#bt{right=R}=T, NewVal) ->
  T#bt{right=insert(R, NewVal)}.
