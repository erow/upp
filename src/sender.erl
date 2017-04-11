%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 四月 2017 17:01
%%%-------------------------------------------------------------------
-module(sender).
-author("erow").

-behaviour(window).
-include("protocol.hrl").
%% API
-export([start_link/1]).
%%-compile([{parse_transform, lager_transform}]).
%% gen_server callbacks
-export([init/1, message/2, confirm/2, loss/2, insert/3, slide/2]).


-define(SERVER, ?MODULE).

-record(state, {
  socket :: integer()
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
  window:start_link(?MODULE, [Socket], []).

init([Socket]) ->
  #state{socket = Socket}.

confirm(_ConfirmList, State = #state{socket = _Socket}) ->
  [udt:send_packet(socket,
    #ack2_packet{timestamp = protocol:timestamp(), ack_sequence = Seq}) ||
    {Seq, _, _} <- _ConfirmList],
  State.

slide(_Len, State = #state{}) ->
  udt:window_change(),
  {ignore, State}.

loss(LossList, State = #state{}) ->
  lager:debug("loss ~p", [LossList]),
  [udt:send_packet(1, #data_packet{
    sequence_number = Seq, timestamp = Timestamp, payload = Data})
    || {Seq, Timestamp, Data} <- LossList],
  State.

insert(Seq, Item, State = #state{socket = Socket}) ->
  lager:debug("send ~p", [Seq]),
  udt:send_packet(Socket,
    #data_packet{sequence_number = Seq,
      destination_socket_id = Socket,
      timestamp = protocol:timestamp(),
      payload = Item}),
  State.

message({send, Data}, State = #state{}) ->
  State.