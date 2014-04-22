-module(tweets_out).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../include/habitpop_records.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,send_tweet/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

send_tweet(Tweet) ->
    gen_server:call(?SERVER,{send_tweet,Tweet}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Fixme) ->
    {ok, Redis} = eredis:start_link(),
    {ok, Redis}.

handle_call({send_tweet, Tweet}, _From, Redis) ->
    {ok, _} = eredis:q(Redis,["PUBLISH","tweets_sent",Tweet]),
    {reply, ok, Redis}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
