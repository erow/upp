%%%-------------------------------------------------------------------
%%% @author erow
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2017 13:35
%%%-------------------------------------------------------------------
-module(basic).
-author("erow").
-compile([{parse_transform, lager_transform}]).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok,_}=upp_app:start(),
  [upp_controler:send(<<"a">>)||_<-lists:seq(1,100)],
  Data=[<<"a">>||_<-lists:seq(1,100)],
  {ok,Data}=upp_controler:recv(),
  ?assert(true).
