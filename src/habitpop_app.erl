-module(habitpop_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("hello from app"),
    habitpop_sup:start_link().

% on tweet ->
% existing user?
%   existing habit?
%     add
%     check if they're due an event (milestone etc)
%   else
%     say hi
% else
%   intro message

%% questions:
% - do hashtags get parsed out already?
% - do mentions?
% - need to subscribe for @ mentions

stop(_State) ->
    ok.
