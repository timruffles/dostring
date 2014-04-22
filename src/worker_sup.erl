-module(worker_sup).
-behaviour(supervisor).

-export([start_link/0,init/1,process_tweet/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

process_tweet(Tweet) ->
    supervisor:start_child(?MODULE,[Tweet]).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    RestartsInPeriod = 5,
    PeriodToCountRestarts = 60,
    RestartStrategy = {simple_one_for_one, RestartsInPeriod, PeriodToCountRestarts},
    %% Empty args - the args of the start_child call are appended
    % { ID, StartFuncMFA, Restart, ShutDown, TypeFromSupervisorOrWorker, Callback}
    TweetHandler = {tweet_handler, {tweet_handler,run,[]},
      transient, brutal_kill, worker, [tweet_handler]},
    Children = [TweetHandler],
    {ok, { RestartStrategy, Children } }.
