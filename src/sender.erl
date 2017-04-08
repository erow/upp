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
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([start_link/0, send/2, ack/2, set_configure/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  controler :: pid(),
  send_window = slide_window:new(10),
  buffer = ets:new(?MODULE, []),
  sequence = 1 :: integer(),
  packet_size = 1024 :: integer(),
  send_rate = 200 :: integer(),
  resend = 400 :: integer(),
  t = 10 :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

send(Ref, Data) ->
  gen_server:cast(Ref, {send, Data}).
ack(Ref, Seq) ->
  gen_server:cast(Ref, {ack, Seq}).

set_configure(Ref, Param, Value) ->
  gen_server:cast(Ref, {set_configure, Param, Value}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link(?MODULE, [self()], []).

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
init([Controler]) ->
  {ok, #state{controler = Controler}}.

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
handle_call(_Request, _From, State) ->
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
handle_cast({ack, Seq}, State = #state{send_window = SendWindow, buffer = Tab, controler = Controler, t = Interval}) ->
  lager:info("~p", [{ack, Seq}]),
  case slide_window:insert(Seq, Seq, SendWindow) of
    {{slide, _RecvList}, NewSendWindow} ->
      ets:take(Tab, Seq),
      {noreply, State#state{send_window = NewSendWindow}, Interval};
    {ok, NewSendWindow} ->
      ets:take(Tab, Seq),
      {noreply, State#state{send_window = NewSendWindow}, Interval};
    {ignore,NewSendWindow}->
      {noreply, State#state{send_window = NewSendWindow}, Interval}
  end;
handle_cast({send, Data}, State = #state{buffer = Buffer,
  packet_size = PackSize,
  sequence = Seq}) ->
  case Data of
    _ when byte_size(Data) < PackSize ->
      send_package(Buffer, {Seq, timestamp(), Data}),
      handle_check(State#state{sequence = Seq + 1});
    <<P1:PackSize/binary, Rest/binary>> ->
      send_package(Buffer, {Seq, timestamp(), P1}),
      handle_cast({send, Rest}, State#state{sequence = Seq + 1})
  end.

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
handle_info(timeout, State = #state{t = Interval}) ->
  handle_check(State);
handle_info(_Info, State) ->
  lager:warning("~p :~p", [_Info, State]),
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

timestamp() ->
  erlang:system_time(milli_seconds).

handle_check(State = #state{t = Interval, buffer = Buffer, resend = Resend, send_window = Window}) ->
%%  ResendList = resend_list(Buffer, timestamp() - Resend),
  Lasn = slide_window:lasn(Window),
  Size = slide_window:size(Window),
  ResendList = ets:select(Buffer, [
    {{'$1', '$2', '_'}, [{'<', '$1', Lasn + Size}, {'=<', '$2' , timestamp()-Resend}], ['$_']}
  ]),
  upp_controler:put_data(ResendList),
  {noreply, State, Interval}.

resend_list(Tab, OutTime) ->
  resend_list(Tab, ets:first(Tab), OutTime, []).

resend_list(_Tab, '$end_of_table', _, Ans) ->
  lists:reverse(Ans);
resend_list(Tab, Seq, OutTime, Ans) ->
  case ets:lookup(Tab, Seq) of
    [Package = {_Seq, TimeStamp, _Data}] when TimeStamp =< OutTime ->
      resend_list(Tab, ets:next(Tab, Seq), OutTime, [Package | Ans]);
    _ ->
      lists:reverse(Ans)
  end.

ack_package(RecvList, Tab, _Controler) ->
  _Data = [ets:take(Tab, Sequence) || Sequence <- RecvList].

send_package(Tab, Package) ->
  lager:debug("~p", [{send_package, Package}]),
  ets:insert(Tab, Package).
