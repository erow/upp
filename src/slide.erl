%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 四月 2017 16:44
%%%-------------------------------------------------------------------
-module(slide).
-author("erow").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  W=slide_window:new(1),
  {ignore,W}=slide_window:insert(0,a,W),
  {ignore,W}=slide_window:insert(2,a,W),
  {{slide,[a]},W1}=slide_window:insert(1,a,W),
  {{slide,[b]},_W2}=slide_window:insert(2,b,W1),
  ?assert(true).

insert_test()->
  W=slide_window:new(2),
  {ok,W1}=slide_window:insert(2,b,W),
  {{slide,[a,b]},_}=slide_window:insert(1,a,W1).

change_test()->
  W=slide_window:new(2),
  {ok,W1}=slide_window:insert(2,b,W),
  {window,{undefine},1,1,0}=slide_window:change_size(1,W1).

change1_test()->
  W=slide_window:new(1),
  {window,{undefine,undefine},2,1,0}=slide_window:change_size(2,W).

transfer_test() ->
  {ok,_}=upp_app:start(),
  [upp_controler:send(<<"a">>)||_<-lists:seq(1,100)],
  Data=[<<"a">>||_<-lists:seq(1,100)],
  {ok,Data}=upp_controler:recv(),
  ?assert(true).