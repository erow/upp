%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 四月 2017 17:43
%%%-------------------------------------------------------------------
-module(gen_upp).
-author("erow").

-behaviour(gen_server).

%% API
-export([start_link/0, recv/0, send/1, listen/1, connect/2]).

% for receiver
-export([get_data/1, ack/1]).
%for sender
-export([put_data/1]).

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
  recv_buffer = <<>>
}).

%%%===================================================================
%%% API
%%%===================================================================

recv() ->
  gen_server:call(?SERVER, recv).

send(Data) ->
  gen_server:call(?SERVER, {send, Data}).

listen(Port) ->
  gen_server:call(?SERVER, {listen, Port}).

connect(Ip, Port) ->
  gen_server:call(?SERVER, {connect, Ip, Port}).


get_data([]) ->
  ok;
get_data(DataList) ->
  gen_server:cast(?SERVER, {get_data, DataList}).

put_data([]) ->
  ok;
put_data(DataList) ->
  gen_server:cast(?SERVER, {put_data, DataList}).


ack(Seq) ->
  gen_server:cast(?SERVER, {ack, Seq}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
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
  {ok, Sender} = sender:start_link(1),
  {ok, Receiver} = receiver:start_link(2),
  {ok, #state{
    sender = Sender,
    receiver = Receiver
  }}.

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
handle_call(recv, _From, State = #state{recv_buffer = RecvBuffer}) ->
  {reply, {ok, RecvBuffer}, State#state{recv_buffer = <<>>}}.

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
handle_info({udp, Socket, IP, InPortNo, Packet}, State) ->
  handle_packet(Packet, State).

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

handle_packet(<<1:1,Seq:31/integer, Data/binary>>, State = #state{receiver = Receiver}) ->
  receiver:recv(Receiver, Seq, Data),
  {noreply, State};
handle_packet(<<0:1,Seq:31/integer>>,State= #state{sender = Sender })->
  sender:ack(Sender,Seq),
  {noreply,State}.



udp_send({S, Ip, Port}, Data) ->
  gen_udp:send(S, Ip, Port, Data);
udp_send(S, Data) ->
  gen_udp:send(S, Data).