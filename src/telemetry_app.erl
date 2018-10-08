-module(telemetry_app).

-behaviour(application).

%% application callbacks
-export([start/2]).
-export([prep_stop/1]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    telemetry_sup:start_link().

prep_stop(_State) ->
    telemetry_snapshot:make(),
    ok.

stop(_State) -> ok.
