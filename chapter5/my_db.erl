-module(e51).

-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([loop/1]).

start() ->
  register(?MODULE, spawn(?MODULE, loop, [[]])),
  ok.

stop() ->
  ?MODULE ! {stop, self()},
  receive {reply, Reply} -> Reply end.

write(Key, Element) ->
  request(?MODULE, {write, Key, Element}).

delete(Key) ->
  request(?MODULE, {delete, Key}).

read(Key) ->
  request(?MODULE, {read, Key}).

match(Element) ->
  request(?MODULE, {match, Element}).

loop(Data) ->
  receive
    {request, Who, Msg} ->
      {Reply, NewData} = handle_msg(Msg, Data),
      reply(Who, Reply),
      loop(NewData);
    {stop, Who} ->
      reply(Who, ok)
  end.

handle_msg(Msg, Data) ->
  case Msg of
    {write, Key, Element} ->
      case lists:keysearch(Key, 1, Data) of
        false ->
          {ok, [{Key, Element}|Data]};
        {value, {_, _}} ->
          {{error, key_already_exists}, Data}
      end;
    {delete, Key} ->
      case lists:keysearch(Key, 1, Data) of
        false ->
          {{error, key_not_exists}, Data};
        {value, {_, _}} ->
          NewData = lists:keydelete(Key, 1, Data),
          {ok, NewData}
        end;
    {read, Key} ->
      case lists:keysearch(Key, 1, Data) of
        false ->
          {{error, key_not_exists}, Data};
        {value, {_, Element}} ->
          {Element, Data}
        end;
    {match, Element} ->
      {match(Data, Element), Data}
  end.

match([], _) -> [];
match([{Key, Element}|Data], Element) ->
  [Key | match(Data, Element)];
match([_|Data], Element)->
  match(Data, Element).
  

request(Who, Msg) ->
  Who ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

reply(Who, Msg) ->
  Who ! {reply, Msg}.
