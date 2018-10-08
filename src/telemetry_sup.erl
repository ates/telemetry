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
    Childs = [
        #{id => telemetry, start => {telemetry, start_link, []}},
        #{id => telemetry_snapshot, start => {telemetry_snapshot, start_link, []}}
    ],
    {ok, {{one_for_one, 10, 5}, Childs}}.
