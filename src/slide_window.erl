%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 四月 2017 9:58
%%%-------------------------------------------------------------------
-module(slide_window).
-author("erow").

-behaviour(gen_server).

%% API
-export([start_link/3, store/3, ack_before/2, remain_capacity/1, unack_list/1, previours_ack/1, ack/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).



-record(state, {
  array_tab :: ets:tab(),
  take_list = [],
  size = 10 :: integer(),
  begin_seq = 1 :: integer(),
  largest_received_seq = 0 :: integer(),
  lagest_store_seq = 0 :: integer(),
  rto = 600 :: integer(),
  last_check_timestamp = 0 :: integer(),
  t = 200 :: integer(),
  mod,
  ext_state
}).

%%%===================================================================
%%% API
%%%===================================================================
-callback confirm(ConfirmList :: [Pair], State :: term()) ->
  NewState
  when
  Pair :: {Seq :: integer(), Timestamp :: integer(), Item :: term()},
  NewState :: term().

-callback loss(LossList :: [Pair], State :: term()) ->
  NewState
  when
  Pair :: {Seq :: integer(), Timestamp :: integer(), Item :: term()},
  NewState :: term().

-callback message(Message :: term(), State :: term()) ->
  NewState :: term().

-callback insert(Seq :: integer(), Item :: term(), State :: term()) ->
  NewState :: term().

-callback init(Args :: []) ->
  State :: term().

-callback slide(Length :: integer(), State :: term()) ->
  {ok, StoreList :: [{Seq :: integer(), term()}], NewState}|
  {ingore, NewState} when NewState :: term().



store(Ref, Seq, Item) ->
  gen_server:call(Ref, {store, Seq, Item}).

ack(Ref, Seq) ->
  gen_server:call(Ref, {ack, Seq}).

ack_before(Ref, Seq) ->
  gen_server:call(Ref, {ack_before, Seq}).

remain_capacity(Ref) ->
  gen_server:call(Ref, remain_capacity).

unack_list(Ref) ->
  gen_server:call(Ref, unack_list).

previours_ack(Ref) ->
  gen_server:call(Ref, previours_ack).


change_config(Ref, Conf, Value) ->
  gen_server:call(Ref, {change_config, Conf, Value}).

get_config(Ref, Conf) ->
  gen_server:call(Ref, {get_config, Conf}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Mod :: atom(), Args :: [], Opt :: []) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Mod, Args, Opt) ->
  gen_server:start_link(?MODULE, [Mod, Args, Opt], []).

%%%===================================================================
%%% gen_server callbacks
%%%%%===================================================================

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
init([Mod, Args, _Opt]) ->
  ExtState = Mod:init(Args),
  {ok, #state{mod = Mod,
    array_tab = ets:new(Mod, [ordered_set]),
    ext_state = ExtState}}.

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
handle_call(previours_ack, From, State = #state{begin_seq = BeginSeq}) ->
  gen_server:reply(From, BeginSeq),
  handle_check(State);
handle_call(remain_capacity, From, State = #state{begin_seq = BeginSeq, size = Size, lagest_store_seq = Lssn}) ->
  gen_server:reply(From, Size + BeginSeq - Lssn - 1),
  handle_check(State);
handle_call(unack_list, From, State = #state{array_tab = Tab}) ->
  gen_server:reply(From, ets:tab2list(Tab)),
  handle_check(State);
handle_call({store, Seq, _Item}, From, State = #state{begin_seq = BeginSeq, size = Size}) when Seq < BeginSeq orelse Seq >= BeginSeq + Size ->
  gen_server:reply(From, ignore),
  handle_check(State);
handle_call({store, Seq, Item}, From, State = #state{lagest_store_seq = Lssn}) ->
  NewExtState = insert_data(Seq, Item, State),
  gen_server:reply(From, ok),
  handle_check(State#state{ext_state = NewExtState, lagest_store_seq = max(Lssn, Seq)});
handle_call({ack, Seq}, From, State = #state{begin_seq = BeginSeq}) when Seq < BeginSeq ->
  lager:warning("smaller ack_seq:~p", [Seq]),
  gen_server:reply(From, ignore),
  handle_check(State);
handle_call({ack, Seq}, From, State = #state{begin_seq = BeginSeq, size = Size}) when Seq >= BeginSeq + Size ->
  lager:warning("bigger ack_seq:~p", [Seq]),
  gen_server:reply(From, wrong),
  handle_check(State);
handle_call({ack_before, Seq}, From, State = #state{begin_seq = BeginSeq, size = Size}) when Seq < BeginSeq orelse Seq >= BeginSeq + Size ->
  gen_server:reply(From, ignore),
  lager:info("ack_before ignore"),
  handle_check(State);
handle_call({ack_before, Seq}, From, State = #state{
  array_tab = Tab,
  begin_seq = BeginSeq,
  take_list = TakeList,
  largest_received_seq = Lrsn}) ->
  gen_server:reply(From, ok),
  case lists:filtermap(fun
                         (Seq1) ->
                           case ets:take(Tab, Seq1) of
                             [] ->
                               false;
                             [Item] ->
                               {true, Item}
                           end
                       end, lists:seq(BeginSeq, Seq)) of
    [] ->
      handle_check(State);
    Item ->
      lager:debug("take,~p", [[S || {S, _, _} <- Item]]),
      handle_check(State#state{take_list = Item ++ TakeList, largest_received_seq = max(Lrsn, Seq - 1)})
  end;
handle_call({ack, Seq}, From, State = #state{
  array_tab = Tab,
  take_list = TakeList,
  largest_received_seq = Lrsn}) ->
  gen_server:reply(From, ok),
  case ets:take(Tab, Seq) of
    [] ->
      handle_check(State);
    [Item] ->
%%      lager:debug("take,~p", [Item]),
      handle_check(State#state{take_list = [Item | TakeList], largest_received_seq = max(Lrsn, Seq)})
  end;
handle_call({change_config, _Conf, _Value}, From, State) ->
  gen_server:reply(From, ignore),
  handle_check(State);
handle_call({get_config, _Conf}, From, State) ->
  gen_server:reply(From, ignore),
  handle_check(State).


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

handle_cast(Message, State = #state{mod = Mod, ext_state = ExtState}) ->
  handle_check(State#state{ext_state = Mod:message(Message, ExtState)}).

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
handle_info(timeout, State) ->
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
  protocol:timestamp().


handle_check(State) ->
  handle_check_slide_window(State).

handle_check_slide_window(State = #state{array_tab = Tab, begin_seq = BeginSeq, take_list = TakeList, largest_received_seq = Lasn}) ->
  % 检查窗口是否移动。
  case ets:first(Tab) of
    BeginSeq ->
      handle_check_retransmit(State);
    Else ->
      NewBeginSeq = case Else of
                      '$end_of_table' ->
                        Lasn + 1;
                      First ->
                        First
                    end,
      case NewBeginSeq of
        BeginSeq ->
          handle_check_retransmit(State);
        _ ->
          ConfirmList = [T || T = {Seq, _, _} <- TakeList, Seq < NewBeginSeq],
          Remain = [T || T = {Seq, _, _} <- TakeList, Seq > NewBeginSeq],
          lager:debug("slide ~p to ~p:~p", [BeginSeq, NewBeginSeq, {largest_received_seq, Lasn}]),
%%          lager:debug("~p", [{{confirmlist, ConfirmList}, {takelist, TakeList}}]),
          #state{mod = Mod, ext_state = ExtState} = State,
          NewExtState = Mod:confirm(ConfirmList, ExtState),
          case Mod:slide(length(ConfirmList), NewExtState) of
            {ignore, NewExtState1} ->
              handle_check_retransmit(State#state{
                begin_seq = NewBeginSeq,
                take_list = Remain,
                ext_state = NewExtState1})
          end
      end
  end.

handle_check_retransmit(State = #state{
  array_tab = Tab,
  rto = RTO}) ->
  % 检查超时的包
  case ets:select(Tab, [
    {{'$1', '$2', '_'},
      [{'=<', '$2', timestamp() - RTO}],
      ['$_']}
  ]) of
    [] ->
      handle_check_end(State);
    Losslist ->
      lager:info("losslist:~w", [[Seq || {Seq, _, _} <- Losslist]]),
      #state{mod = Mod, ext_state = ExtState} = State,
      NewExtState = Mod:loss(Losslist, ExtState),
      handle_check_end(State#state{ext_state = NewExtState})
  end.

handle_check_end(State = #state{t = Interval}) ->
  {noreply, State#state{last_check_timestamp = timestamp()}, Interval}.

insert_data(Seq, Item, #state{
  mod = Mod, ext_state = ExtState, array_tab = Tab
}) ->
  ets:insert(Tab, {Seq, timestamp(), Item}),
  Mod:insert(Seq, Item, ExtState).