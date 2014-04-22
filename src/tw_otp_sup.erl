-module(tw_otp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = {one_for_one, 0, 1},

    StoreConfig = {app_config:redis_config(),app_config:pg_config()},
    OutConfig = {app_config:redis_config()},
    InConfig = {app_config:redis_sub_config()},

    % application processes
    WorkerSupervisor = {worker_sup, {worker_sup,start_link,[]},
       permanent, brutal_kill, supervisor, [worker_sup]},
    TweetStore = {tw_store, {tw_store,start_link,[StoreConfig]},
      permanent, brutal_kill, worker, [tw_store]},
    TweetsIn = {tweets_in, {tweets_in,start_link,[InConfig]},
       permanent, brutal_kill, worker, [tweets_in]},
    TweetsOut = {tweets_out, {tweets_out,start_link,[OutConfig]},
       permanent, brutal_kill, worker, [tweets_out]},
    TwitterListener = {twitter_listener, {twitter_listener,listen,[]},
      permanent, brutal_kill, worker, [twitter_listener]},

    % depedencies
    Pg = {pgsql_connection_sup, {pgsql_connection_sup,start_link,[]},
       permanent, brutal_kill, supervisor, [pgsql_connection_sup]},

    
    {ok, { RestartStrategy,
      [Pg,
       WorkerSupervisor,TweetStore,TwitterListener,TweetsIn,TweetsOut]} }.


