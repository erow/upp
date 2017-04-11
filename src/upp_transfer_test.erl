%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 四月 2017 16:39
%%%-------------------------------------------------------------------
-module(upp_transfer_test).
-author("erow").

-behaviour(gen_server).
-include("protocol.hrl").
%% API
-export([start_link/0, send_data/2]).

-define(interval, 50).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  socket_tab = ets:new(?MODULE, []),
  last_check_time :: integer(),
  send_queue = queue:new(),
  rate = 20000,% byte
  count = 0,%byte
  local,
  remote
}).

%%%===================================================================
%%% API
%%%===================================================================
%%send_packet(Socket, Packet) ->
%%  gen_server:cast(?SERVER, {send_packet, Socket, Packet}).
send_data(Socket, RawData) ->
  gen_server:cast(?SERVER, {send_data, Socket, RawData}).
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
  State = #state{last_check_time = protocol:timestamp()},
  ets:insert(State#state.socket_tab, {{"127.0.0.1", 1}, udt}),
  {ok, State, ?interval}.

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
handle_call(forbidden, _From, State) ->
  {reply, ok, State}.

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
handle_cast({send_data, _Socket, RawData}, State = #state{send_queue = Queue}) ->
  handle_check(State#state{send_queue = queue:in({_Socket, RawData}, Queue)}).

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
handle_info({udp, 1, "127.0.0.1", 1, RawData}, State = #state{socket_tab = Tab}) ->
  case ets:lookup(Tab, {"127.0.0.1", 1}) of
    [{{"127.0.0.1", 1}, Ref}] ->
      udt:recv_packet(Ref, RawData)
  end,
  handle_check(State);
handle_info(_Info, State) ->
  {noreply, State, ?interval}.

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

handle_check(State = #state{last_check_time = LastTime,
  count = Count, rate = Rate,
  send_queue = Queue}) ->
  Now = protocol:timestamp(),
  if
    LastTime > Now + 1000 ->
      handle_check(State#state{last_check_time = Now, count = 0});
    Count < Rate ->
      case queue:out(Queue) of
        {{value, {_, RawData}}, Q1} ->
          self() ! {udp, 1, "127.0.0.1", 1, RawData},
          handle_check(State#state{count = Count + 1, send_queue = Q1});
        {empty, _} ->
          {noreply, State, ?interval}
      end;
    true ->
      {noreply, State, ?interval}
  end.