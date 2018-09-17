-module(telemetry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(telemetry, [
        public,
        named_table,
        {write_concurrency, true}
    ]),
    {ok, {{one_for_one, 10, 5}, [#{id => telemetry, start => {telemetry, start_link, []}}]}}.
