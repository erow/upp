%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 四月 2017 21:38
%%%-------------------------------------------------------------------
-module(udt).
-author("erow").

-behaviour(gen_server).
-include("protocol.hrl").
%% API
-export([start_link/0, send_packet/2, recv_packet/2, send/1, recv/0, listen/1, connect/2, window_change/0, get_data/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  sender,
  receiver,
  recv_buffer = <<>>,
  send_buffer = <<>>,
  sequence = 1 :: integer(),
  packet_size = 1024 :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================
listen(Port) ->
  gen_server:call(?SERVER, {listen, Port}).

connect(Ip, Port) ->
  gen_server:call(?SERVER, {connect, Ip, Port}).

%%这里多路复用的时候需要用到。不打算实现了。
send_packet(_S, Packet) ->
  gen_server:cast(?SERVER, {send_packet, Packet}).

recv_packet(_Ref, Packet) ->
  gen_server:cast(?SERVER, {recv_packet, Packet}).

send(Data) ->
  gen_server:call(?SERVER, {send, Data}).

recv() ->
  gen_server:call(?SERVER, recv).

window_change() ->
  gen_server:cast(?SERVER, window_change).

get_data(DataList) ->
  gen_server:cast(?SERVER, {get_data, DataList}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, Receiver} = receiver:start_link(2),
  {ok, #state{receiver = Receiver}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({connect, Ip, Port}, _From, State) ->
  Socket = 1,
  {ok, Sender} = sender:start_link(Socket),
  udp_transfer:allocate(Socket, {Ip, Port}),
  send_packet(Socket, #handshake_packet{
    timestamp = protocol:timestamp(),
    initial_sequence_number = 1
  }),
  {reply, ok, State#state{sender = Sender}};
handle_call(recv, _From, State = #state{recv_buffer = RecvBuffer}) ->
  {reply, {ok, RecvBuffer}, State#state{recv_buffer = <<>>}};
handle_call({send, Data}, From, State = #state{send_buffer = Buffer}) ->
  gen_server:reply(From, ok),
  handle_cast(window_change, State#state{send_buffer = <<Buffer/binary, Data/binary>>}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(window_change, State = #state{
  sender = Sender,
  sequence = Seq,
  packet_size = PacketSize,
  send_buffer = SendBuffer}) ->
  Len = slide_window:remain_capacity(Sender),
  {Rest, StoreList} = split_data(Len, SendBuffer, PacketSize, []),
  NewSeq = lists:foldl(fun(Data, S) ->
    slide_window:store(Sender, S, Data),
    S + 1 end, Seq, StoreList),
  {noreply, State#state{sequence = NewSeq, send_buffer = Rest}};
handle_cast({send_packet, Packet}, State) ->
  handle_send_packet(Packet, State);
handle_cast({recv_packet, RawData}, State) ->
  try protocol:decode_packet(RawData) of
    Packet ->
      lager:info("recv_packet ~p", [element(1, Packet)]),
      handle_recv_packet(Packet, State)
  catch
    Exception ->
      lager:warning("decode wrong ~p", [Exception])
  end;
handle_cast({get_data, DataList}, State = #state{recv_buffer = RecvBuffer}) ->
  BinaryData = list_to_binary(DataList),
  {noreply, State#state{recv_buffer = <<RecvBuffer/binary, BinaryData/binary>>}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



handle_send_packet(Packet, State = #state{}) ->
  lager:debug("~p", [{send_packet, element(1, Packet)}]),
  lager:debug("~p", [{send_packet, Packet}]),
  Socket = element(3, Packet),
  upp_transfer_test:send_data(Socket, protocol:encode_packet(Packet)),
  {noreply, State}.

handle_recv_packet(#data_packet{sequence_number = Seq, payload = Data}, State = #state{receiver = Receiver}) ->
  slide_window:store(Receiver, Seq, Data),
  {noreply, State};
handle_recv_packet(#ack2_packet{ack_sequence = AckSeq}, State = #state{receiver = Receiver}) ->
  slide_window:ack(Receiver, AckSeq),
  {noreply, State};
handle_recv_packet(#ack_packet{previous_packets_received = Seq}, State = #state{sender = Sender}) ->
  slide_window:ack(Sender, Seq),
  {noreply, State};
handle_recv_packet(#handshake_packet{}, State) ->
  {noreply, State}.

split_data(0, Data, _PackSize, Ans) ->
  {Data, lists:reverse(Ans)};
split_data(Size, Data, PacketSize, Ans) ->
  case Data of
    <<>> ->
      {<<>>, lists:reverse(Ans)};
    Data when byte_size(Data) =< PacketSize ->
      {<<>>, lists:reverse([Data | Ans])};
    <<P1:PacketSize/binary, Rest/binary>> ->
      split_data(Size - 1, Rest, PacketSize, [P1 | Ans])
  end.

-include_lib("eunit/include/eunit.hrl").

split_test() ->
  Data = <<1:32>>,
  {<<>>, [Data]} = split_data(1, Data, 4, []),
  {<<1:16>>, [<<0:16>>]} = split_data(1, Data, 2, []),
  {<<>>, [<<0:16>>, <<1:16>>]} = split_data(2, Data, 2, []),
  {<<>>, [<<0:16>>, <<1:16>>]} = split_data(3, Data, 2, []),
  {<<>>, []} = split_data(3, <<>>, 2, []).