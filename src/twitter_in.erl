-module(twitter_in).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([listen/0]).


listen () ->
  io:format("Ok let's get some tweets~n"),
  {ok, Sub} = eredis_sub:start_link(),
  % R = eredis:q(Sub,["KEYS","*"]),
  % io:format("GOT ~p~n",[R]),
  spawn_link(fun () ->
    ok = eredis_sub:controlling_process(Sub),
    ok = eredis_sub:subscribe(Sub, [<<"tweets">>]),
    io:format("subbed ~p~n",[eredis_sub:channels(Sub)]),
    receiver(Sub),
    io:format("DONE!!~n")
  end).

receiver(Sub) ->
    receive
        Msg ->
            io:format("received ~p~n", [Msg]),
            eredis_sub:ack_message(Sub),
            receiver(Sub)
    end.
  


handle_tweet (Tweet) ->
  ok.


month_name_to_erlang_month (Name) ->
  case Name of
    "Jan" -> 1;
    "Feb" -> 2;
    "Mar" -> 3;
    "Apr" -> 4;
    "May" -> 5;
    "Jun" -> 6;
    "Jul" -> 7;
    "Aug" -> 8;
    "Sep" -> 9;
    "Oct" -> 10;
    "Nov" -> 11;
    "Dec" -> 12
  end.

% Mon Sep 24 03:35:21 +0000 2012
parse_twitter_date (Date) ->
  {_,[_,Month,Day,Hour,Min,Seconds,TimePlus,Timezone,Year],_} = io_lib:fread("~s ~s ~d ~d:~d:~d ~-~d ~d",Date),
  AsDate = {{Year,month_name_to_erlang_month(Month),Day},{Hour,Min,Seconds}},
  calendar:date_to_gregorian_seconds(AsDate) + TimePlus * Timezone * ?HOUR.
