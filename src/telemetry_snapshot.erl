-module(telemetry_snapshot).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([make/0]).
-export([restore/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {
    interval = 0 :: non_neg_integer(),
    tref         :: undefined | reference()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case application:get_env(telemetry, snapshot) of
        {ok, #{interval := Seconds}} ->
            restore(),
            State = #state{
                interval = Seconds * 1000,
                tref     = erlang:send_after(Seconds * 1000, ?MODULE, make_snapshot)
            },
            {ok, State};
        _ -> {ok, #state{}}
    end.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(make_snapshot, #state{interval = Interval} = State) ->
    erlang:cancel_timer(State#state.tref),
    make(),
    {noreply, State#state{tref = erlang:send_after(Interval, ?MODULE, make_snapshot)}}.

-spec make() -> ok | {error, Reason :: atom()}.
make() ->
    case application:get_env(telemetry, snapshot) of
        {ok, #{file := File}} ->
            Metrics = [
                Metric || {Name, _Type, _Value} = Metric <- ets:tab2list(telemetry),
                          hd(Name) =/= vm
            ], %% skip vm metrics
            case file:write_file(File, term_to_binary(Metrics), [sync]) of
                ok -> ok;
                {error, Reason} = Error ->
                    logger:error("can't make metrics snapshot, reason: ~s", [
                        file:format_error(Reason)
                    ]),
                    Error
            end;
        _ -> {error, not_configured}
    end.

-spec restore() -> ok | {error, Reason :: atom()}.
restore() ->
    case application:get_env(telemetry, snapshot) of
        {ok, #{file := File}} ->
            case file:read_file(File) of
                {ok, Metrics} ->
                    F = fun
                            ({Name, Type, Extra}) when Type =:= counter; Type =:= function ->
                                telemetry:new(Name, Type, Extra);
                            ({Name, histogram, Extra}) ->
                                telemetry:new(Name, histogram, element(3, Extra))
                        end,
                    lists:foreach(F, binary_to_term(Metrics));
                {error, Reason} = Error ->
                    logger:warning("can't restore metric snapshot, reason: ~s", [
                        file:format_error(Reason)
                    ]),
                    Error
            end;
        _ -> {error, not_configured}
    end.
