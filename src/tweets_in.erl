-module(tweets_in).
-behaviour(gen_fsm).

-include_lib("eunit/include/eunit.hrl").
-include("../include/habitpop_records.hrl").

-define(SERVER, ?MODULE).

-record(state,{tweet=none,sub}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,get_tweet/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1,handle_info/3, terminate/3,
         code_change/4,handle_event/3,handle_sync_event/4,
         no_tweet/3,has_tweet/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Config) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Config, []).

get_tweet() ->
    gen_fsm:sync_send_event(?SERVER,get_tweet,infinity).


%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init({RedisConfig}) ->
    {ok, Sub} = erlang:apply(eredis_sub,start_link,RedisConfig),
    ok = eredis_sub:controlling_process(Sub),
    ok = eredis_sub:subscribe(Sub, ["tweets"]),
    ok = eredis_sub:ack_message(Sub),
    io:format("subbed ~p~n",[eredis_sub:channels(Sub)]),
    {ok, no_tweet, #state{sub=Sub}}.

no_tweet(get_tweet,_From,Current = #state{sub=Sub}) ->
  {reply,wait_for_tweet(Sub),no_tweet,Current}.

has_tweet(get_tweet,_From,#state{sub=Sub,tweet=Tweet}) ->
  eredis_sub:ack_message(Sub),
  {reply,Tweet,no_tweet,#state{sub=Sub}}.

handle_info({message,_,TweetText,_},no_tweet,#state{sub=Sub,tweet=none}) ->
  io:format("GOT TWEET\n"),
  {next_state,has_tweet,#state{sub=Sub,tweet=format_tweet(TweetText)}};
handle_info({subscribed,_,_},State,StateData = #state{sub=Sub}) ->
  io:format("SUBSCRIBED\n"),
  eredis_sub:ack_message(Sub),
  {next_state,State,StateData};
handle_info(Sth,State,StateData) ->
  io:format("STH ~p\n",[Sth]),
  {next_state,State,StateData}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, ok, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
wait_for_tweet(Sub) ->
  io:format("WAITING\n"),
  receive
    {message,_,TweetText,_} ->
      io:format("REPLY WITH TWEET\n"),
      Formatted = format_tweet(TweetText),
      eredis_sub:ack_message(Sub),
      Formatted;
    {subscribed,_,_} ->
      io:format("SUBSCRIBED\n"),
      eredis_sub:ack_message(Sub);
    Sth ->
      io:format("LOOP GOT STH ELSE ~p\n",[Sth]),
      wait_for_tweet(Sub)
  end.

format_tweet(TweetText) ->
    io:format("got tweet ~p~n",[TweetText]),
    {Tweet} = jiffy:decode(TweetText),
    [
      {<<"id">>,Id},
      {<<"text">>,Text},
      {<<"created_at">>,CreatedAt},
      {<<"hashtags">>,Hashtags},
      {<<"user_id">>,UserId},
      {<<"screen_name">>,ScreenName}
    ] = Tweet,
    #tweet{
      id=to_int(Id),
      username=ScreenName,
      hashtags=lists:map(fun binary_to_list/1,Hashtags),
      text=Text,
      gregorian_seconds=parse_twitter_date(binary_to_list(CreatedAt)),
      user_id=to_int(UserId)}.

  
to_int (B) ->
  {Int,[]} = string:to_integer(binary:bin_to_list(B)),
  Int.

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
  calendar:datetime_to_gregorian_seconds(AsDate) + TimePlus * Timezone * ?HOUR.


%% ------------------------------------------------------------------
%% TESTS
%% ------------------------------------------------------------------

