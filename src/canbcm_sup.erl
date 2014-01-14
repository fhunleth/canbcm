-module(canbcm_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Start the supervisor.
%% @end
-spec(start_link(string()) -> {ok, pid()} | {error, reason}).
start_link(Device) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Device).

%% @doc
%% Stop the supervisor.
%% @end
stop() ->
    exit(whereis(?MODULE), shutdown).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Device) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    Child = {Device, {canbcm, start_link, [Device]},
	      Restart, Shutdown, Type, [canbcm]},

    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
