%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 四月 2017 17:01
%%%-------------------------------------------------------------------
-module(receiver).
-author("erow").

-behaviour(slide_window).
-include("protocol.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, message/2, confirm/2, loss/2, insert/3, slide/2]).


-define(SERVER, ?MODULE).

-record(state, {
  socket :: integer(),
  buffer = <<>>,
  sequence = 1 :: integer(),
  packet_size = 1024 :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================
send(Ref, Data) ->
  gen_server:cast(Ref, {send, Data}).

%%%===================================================================
%%%  callbacks
%%%===================================================================
-spec(start_link(Socket :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Socket) ->
  slide_window:start_link(?MODULE, [Socket], []).

init([Socket]) ->
  #state{socket = Socket}.

confirm(ConfirmList, State = #state{socket = _Socket}) ->
  DataList = [Data || {_, _, Data} <- ConfirmList],
  Seq = lists:max([Seq || {Seq, _, _} <- ConfirmList]),
  lager:debug("confirm:~p", [[Seq || {Seq, _, _} <- ConfirmList]]),
  udt:get_data(DataList),
  State.

slide(Len, State = #state{
  sequence = Seq
}) ->
  NewSeq = Seq + Len,
  {ignore, State#state{sequence = NewSeq}}.

loss(LossList, State = #state{socket = Socket}) ->
  lager:debug("loss ~w", [{[Seq || {Seq, _, _} <- LossList]}]),
%%  LossSeq = [Seq || {Seq, _Timestamp, _Data} <- LossList],
%%  udt:send_packet(Socket, #nak{
%%    destination_socket_id = Socket,
%%    timestamp = protocol:timestamp(),
%%    loss_list = LossSeq}),
  State.

insert(Seq, _Item, State = #state{socket = Socket}) ->
  lager:debug("recv ~w", [Seq]),
  udt:send_packet(Socket,
    #ack_packet{destination_socket_id = Socket,
      timestamp = protocol:timestamp(),
      previous_packets_received = Seq}),
  State.

message(no, State = #state{buffer = Buffer}) ->
  State.




