-module(telemetry_SUITE).

%% ct callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([end_per_testcase/2]).

-export([counters/1]).
-export([counters_load_test/1]).
-export([functions/1]).
-export([histograms/1]).
-export([histograms_load_test/1]).
-export([values/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        counters,
        counters_load_test,
        functions,
        histograms,
        histograms_load_test,
        values
    ].

init_per_suite(Config) ->
    application:start(telemetry),
    Config.

end_per_suite(Config) ->
    application:stop(telemetry),
    Config.

end_per_testcase(_, Config) ->
    [telemetry:delete(M) || M <- telemetry:metrics()],
    Config.

counters(_Config) ->
    0 = telemetry:new([test], counter),
    11 = telemetry:update([test], counter, 11),
    ok = telemetry:reset([test]),
    ok = telemetry:reset([test1]),
    ok = telemetry:reset([test], 10),
    10 = telemetry:value([test]),
    undefined = telemetry:value([test1]),
    true = telemetry:is_metric([test]),
    ok = telemetry:delete([test]),
    ok = telemetry:delete([test1]),
    false = telemetry:is_metric([test]).

counters_load_test(_Config) ->
    0 = telemetry:new([test], counter),
    [
        [spawn(fun() -> telemetry:update([test], counter, 1) end) || _ <- lists:seq(1, 10000)] || _ <- lists:seq(1, 100)
    ],
    timer:sleep(200),
    1000000 = telemetry:value([test]).

functions(_Config) ->
    ok = telemetry:new([test], function, fun() -> 10 end),
    10 = telemetry:value([test]),
    ok = telemetry:new([test1], function, {lists, sum, [[1, 2, 3]]}),
    6 = telemetry:value([test1]),
    ok = telemetry:new([test2], function, fun() -> throw(bang) end),
    undefined = telemetry:value([test2]),
    ok = telemetry:reset([test1]),
    true = telemetry:is_metric([test1]),
    ok = telemetry:delete([test]),
    false = telemetry:is_metric([test]),
    ok = telemetry:new([test], function, fun() -> [{a, 1}, {b, 2}] end),
    [{a, 1}, {b, 2}] = telemetry:value([test]).

histograms(_Config) ->
    ok = telemetry:new([test], histogram),
    ok = telemetry:new([test], histogram),
    ok = telemetry:update([test], histogram, 1),
    #{n := 1} = telemetry:value([test]),
    ok = telemetry:reset([test]),
    #{n := 0} = telemetry:value([test]),
    ok = telemetry:delete([test]),
    ok = telemetry:new([test], histogram, #{window => 1}),
    ok = telemetry:update([test], histogram, 1),
    #{n := 1} = telemetry:value([test]),
    timer:sleep(2000),
    #{n := 0} = telemetry:value([test]),
    ok = telemetry:delete([test]),
    ok = telemetry:update([test], histogram, 10),
    false = telemetry:is_metric([test]).

histograms_load_test(_Config) ->
    ok = telemetry:new([test], histogram),
    [
        [spawn(fun() -> telemetry:update([test], histogram, X) end) || X <- lists:seq(1, 10000)]
        || _ <- lists:seq(1, 100)
    ],
    timer:sleep(200),
    #{n := 1000000} = telemetry:value([test]).

values(_Config) ->
    [
        0 = telemetry:new([test, N], counter) || N <- lists:seq(1, 10)
    ],
    10 = length(telemetry:values()),
    [telemetry:delete(M) || M <- telemetry:metrics()],
    ok = telemetry:new([test], function, fun() -> throw(bang) end),
    [] = telemetry:values(),
    ok = telemetry:delete([test]),
    ok = telemetry:new([test], function, fun() -> [{a, 1}, {b, 2}] end),
    [
        {[test, a], 1},
        {[test, b], 2}
    ] = telemetry:values().
