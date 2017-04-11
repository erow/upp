%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 十一月 2016 22:42
%%%-------------------------------------------------------------------
-module(slide_window).
-author("erow").
-include("protocol.hrl").
%% API
-export([init/1, insert/3, change_size/2, lasn/1, vacancy_seqs/1, get_values/1, size/1]).


-record(state, {
  array :: ets:tab(),
  size :: integer(),
  rto = 400 :: integer(),
  last_check_timestamp = 0 :: integer(),
  begin_seq = 1 :: integer(),
  largest_received_seq = 0 :: integer()
}).

%%%=========================================================================
%%%  API
%%%=========================================================================

store(Sequence, Item, Window) ->
  Window.

-spec init(Size :: integer()) -> Window :: #state{}.
init(Size) ->
  #state{array = ets:new(?MODULE, []),
    size = Size}.


change_size(Size, Window = #state{size = Size}) ->
  Window;
change_size(NewSize, Window = #state{size = Size, array = Array, begin_seq = BeginSeq}) ->
  NewArray = erlang:make_tuple(NewSize, undefine),
  NewArray1 = set_element(BeginSeq, BeginSeq + min(NewSize, Size), NewArray, NewSize, Array, Size),
  Window#state{size = NewSize,
    array = NewArray1,
    largest_received_seq = cal_lrsn(BeginSeq + NewSize - 1, BeginSeq - 1, NewArray1, NewSize)}.

cal_lrsn(Seq, Seq, _Array, _) ->
  Seq;
cal_lrsn(Seq, EndSeq, Array, Size) ->
  case erlang:element(pos(Seq, Size), Array) of
    undefine ->
      cal_lrsn(Seq - 1, EndSeq, Array, Size);
    _ ->
      Seq
  end.

set_element(Seq, Seq, NewArray, _, _, _) ->
  NewArray;
set_element(Seq, EndSeq, NewArray, Size1, OldArray, Size2) ->
  NewArray1 = erlang:setelement(
    pos(Seq, Size1),
    NewArray,
    element(pos(Seq, Size2), OldArray)),
  set_element(Seq + 1, EndSeq, NewArray1, Size1, OldArray, Size2).

-spec insert(Seq, Value, Window) -> {ok|ignore|{slide, RecvList}, NewWindow} when
  Seq :: integer(),
  Value :: term(),
  RecvList :: [term()],
  Window :: #state{},
  NewWindow :: #state{}.

insert(Seq, _Value, Window = #state{begin_seq = BeginSeq, size = Size}) when Seq >= BeginSeq + Size; Seq < BeginSeq ->
  {ignore, Window};
insert(Seq, Value, Window = #state{begin_seq = BeginSeq, largest_received_seq = Lrsn, array = Array, size = Size}) ->
  case element(pos(Seq, Size), Array) of
    undefine ->
      NewArray = setelement(pos(Seq, Size), Array, Value),
      if
        BeginSeq =:= Seq ->
          {RecvList, NewWindow} = slide([], Window#state{array = NewArray, largest_received_seq = max(Lrsn, Seq)}),
          {{slide, RecvList},
            NewWindow};
        true ->
          {ok,
            Window#state{largest_received_seq = max(Seq, Lrsn), array = NewArray}}
      end;
    _ ->
      {ignore, Window}
  end.


slide(RecvList, Window = #state{begin_seq = BeginSeq, largest_received_seq = Lrsn}) when BeginSeq == Lrsn + 1 ->
  {lists:reverse(RecvList), Window};
slide(RecvList, Window = #state{begin_seq = BeginSeq, array = Array, size = Size}) ->
  case erlang:element(pos(BeginSeq, Size), Array) of
    undefine ->
      {lists:reverse(RecvList), Window};
    Item ->
      slide([Item | RecvList], Window#state{
        begin_seq = BeginSeq + 1,
        array = erlang:setelement(pos(BeginSeq, Size), Array, undefine)
      })
  end.

lasn(#state{begin_seq = Lasn}) ->
  Lasn.

size(#state{size = Size}) ->
  Size.

vacancy_seqs(#state{array = Array, largest_received_seq = Lrsn, begin_seq = BeginSeq, size = Size}) ->
  vacancy_seqs(Lrsn, BeginSeq, Array, Size).

vacancy_seqs(Seq, EndSeq, _Array, _) when Seq < EndSeq ->
  [];
vacancy_seqs(Seq, EndSeq, _Array, _) when Seq == EndSeq ->
  [Seq];
vacancy_seqs(Seq, EndSeq, Array, Size) ->
  case erlang:element(pos(Seq, Size), Array) of
    undefine ->
      [Seq | vacancy_seqs(Seq - 1, EndSeq, Array, Size)];
    _ ->
      vacancy_seqs(Seq - 1, EndSeq, Array, Size)
  end.

get_values(#state{array = Array, largest_received_seq = Lrsn, begin_seq = BeginSeq, size = Size}) ->
  get_values(Lrsn, BeginSeq, Array, Size).

get_values(Seq, EndSeq, _Array, _) when Seq =< EndSeq ->
  [];
get_values(Seq, EndSeq, Array, Size) ->
  case erlang:element(pos(Seq, Size), Array) of
    undefine ->
      get_values(Seq - 1, EndSeq, Array, Size);
    Value ->
      [Value | get_values(Seq - 1, EndSeq, Array, Size)]
  end.

pos(Seq, Size) ->
  (Seq rem Size) + 1.

-include_lib("eunit/include/eunit.hrl").
a_test() ->
  A = init(1),
  ?assertMatch({["hello"], {2, 1, {"hello"}, 1}}, insert(1, "hello", A)).

