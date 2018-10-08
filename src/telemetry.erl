-module(telemetry).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([new/2]).
-export([new/3]).
-export([update/3]).
-export([delete/1]).
-export([reset/1]).
-export([reset/2]).
-export([is_metric/1]).
-export([value/1]).
-export([metrics/0]).
-export([values/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-type metric_name() :: [term()].
-type histogram_opts() :: #{window => non_neg_integer()}.

-export_type([metric_name/0]).
-export_type([histogram_opts/0]).

-record(histogram, {
    tid = ets:new(?MODULE, [
        public,
        duplicate_bag,
        {write_concurrency, true}
    ])                     :: ets:tid(),
    opts = #{window => 60} :: histogram_opts(),
    trim_proc              :: undefined | pid()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec new(Name :: metric_name(), Type :: counter | histogram) -> integer().
new(Name, counter) ->
    new(Name, counter, 0);

new(Name, histogram) ->
    new(Name, histogram, #{window => 60}).

-spec new(
    Name  :: metric_name(),
    Type  :: counter | histogram | function,
    Value :: integer() | function() | tuple() | histogram_opts()
) -> integer() | ok.
new(Name, counter, Value) ->
    ets:insert(?MODULE, {Name, counter, Value}),
    Value;

new(Name, function, Fun) when is_function(Fun); tuple_size(Fun) =:= 3 ->
    ets:insert(?MODULE, {Name, function, Fun}),
    ok;

new(Name, histogram, Opts) ->
    gen_server:call(?MODULE, {create_histogram, Name, Opts}).

-spec update(
    Name  :: metric_name(),
    Type  :: counter | histogram,
    Value :: integer()
) -> integer() | ok.
update(Name, counter, Value) ->
    ets:update_counter(?MODULE, Name, {3, Value}, {Name, counter, 0});

update(Name, histogram, Value) ->
    case ets:lookup(?MODULE, Name) of
        [] -> ok;
        [{Name, histogram, #histogram{tid = TID}}] ->
            ets:insert(TID, {erlang:system_time(seconds), Value}),
            ok
    end.

-spec delete(Name :: metric_name()) -> ok.
delete(Name) ->
    case ets:take(?MODULE, Name) of
        [] -> ok;
        [{Name, Type, _Value}] when Type =:= counter; Type =:= function ->
            ok;
        [{Name, histogram, #histogram{tid = TID, trim_proc = Pid}}] ->
            Pid ! stop,
            ets:delete(TID),
            ok
    end.

-spec reset(Name :: metric_name()) -> ok.
reset(Name) ->
    reset(Name, 0).

-spec reset(Name :: metric_name(), Value :: integer()) -> ok.
reset(Name, Value) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, counter, _Value}] ->
            ets:update_element(?MODULE, Name, [{3, Value}]),
            ok;
        [{Name, histogram, #histogram{tid = TID}}] ->
            ets:delete_all_objects(TID),
            ok;
        _ -> ok
    end.

-spec is_metric(Name :: metric_name()) -> boolean().
is_metric(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] -> false;
        [{Name, _Type, _Value}] -> true
    end.

-spec value(Name :: metric_name()) -> integer() | map() | list() | undefined.
value(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] -> undefined;
        [{Name, counter, Value}] ->
            Value;
        [{Name, histogram, #histogram{tid = TID, opts = #{window := Window}}}] ->
            Delta = erlang:system_time(seconds) - Window,
            Values = ets:select(TID, [{{'$1','$2'}, [{'>=', '$1', Delta}], ['$2']}]),
            format_histogram_values(Values);
        [{Name, function, Fun}] ->
            try
                run_function(Fun)
            catch
                _:Reason ->
                    logger:error("can't get value of ~p metric, reason: ~p", [Name, Reason]),
                    undefined
            end
    end.

-spec metrics() -> [metric_name()].
metrics() ->
    ets:foldl(fun(E, Acc) -> [element(1, E) | Acc] end, [], ?MODULE).

-spec values() -> [{metric_name(), integer() | map()}].
values() ->
    F =
        fun(Name, Acc) ->
            case value(Name) of
                undefined -> Acc;
                Value when is_list(Value) ->
                    [{Name ++ [DP], N} || {DP, N} <- Value] ++ Acc;
                Value ->
                    [{Name, Value} | Acc]
            end
        end,
    lists:foldl(F, [], metrics()).

init([]) ->
    Metrics = [
        {[vm, ports, count],           {erlang, system_info, [port_count]}},
        {[vm, process, count],         {erlang, system_info, [process_count]}},
        {[vm, memory, ets],            {erlang, memory, [ets]}},
        {[vm, memory, binary],         {erlang, memory, [binary]}},
        {[vm, memory, atom],           {erlang, memory, [atom]}},
        {[vm, memory, total],          {erlang, memory, [total]}},
        {[vm, memory, processes],      {erlang, memory, [processes]}},
        {[vm, memory, processes_used], {erlang, memory, [processes_used]}}
    ],
    lists:foreach(fun({Name, Fun}) -> new(Name, function, Fun) end, Metrics),
    {ok, []}.

handle_call({create_histogram, Name, Opts}, _From, State) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            Histogram = #histogram{opts = Opts},
            TrimProc = spawn(fun() -> trim_histogram(Histogram) end),
            ets:insert(?MODULE, {Name, histogram, Histogram#histogram{trim_proc = TrimProc}});
        _ -> ok
    end,
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

run_function(Fun) when is_function(Fun) -> Fun();
run_function({M, F, A}) -> erlang:apply(M, F, A).

trim_histogram(#histogram{tid = TID, opts = #{window := Window}} = Histogram) ->
    receive
        _ -> ok
    after Window * 1000 div 2 ->
        try
            Delta = erlang:system_time(seconds) - Window,
            ets:select_delete(TID, [{{'$1', '_'}, [{'<', '$1', Delta}], [true]}]),
            trim_histogram(Histogram)
        catch
            error:badarg ->
                logger:error("can't trim histogram table ~p", [TID])
        end
    end.

format_histogram_values(Value) ->
    Length = length(Value),
    SortedValues = lists:sort(Value),
    #{
        n    => Length,
        min  => histogram_min(SortedValues),
        mean => histogram_mean(SortedValues, Length),
        max  => histogram_max(SortedValues),
        50   => percentile(SortedValues, Length, 50),
        75   => percentile(SortedValues, Length, 75),
        80   => percentile(SortedValues, Length, 80),
        90   => percentile(SortedValues, Length, 90),
        95   => percentile(SortedValues, Length, 95),
        99   => percentile(SortedValues, Length, 99),
        999  => percentile(SortedValues, Length, 99.9)
    }.

percentile(SortedValues, Length, Percentile) ->
    case round(Percentile * Length / 100) of
        0 -> 0;
        Pos -> lists:nth(Pos, SortedValues)
    end.

histogram_min([]) -> 0;
histogram_min([Value | _Rest]) -> Value.

histogram_mean([], _Length) -> 0;
histogram_mean(List, Length) -> lists:sum(List) div Length.

histogram_max([]) -> 0;
histogram_max(List) -> lists:max(List).
