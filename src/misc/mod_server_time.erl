-module(mod_server_time).
-behaviour(gen_server).

-define(DAILY_REFRESH_TIME, 18000).         %% 每天刷新时间点,凌晨5点(18000秒)

-export([
    start_link/0,
    info/0
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(CLOCK, 100).
-define(CLOCK_FOR_DAILY, 1000).

info() ->
    [
     ets:info(ets_timer),
     ets:tab2list(ets_timer)
    ].

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(ets_timer, [set, protected, named_table, {read_concurrency, true}]),
    NowSecs = erlang:now(),
    NowUniversalTime = calendar:now_to_universal_time(NowSecs),
    NowLocalTime = calendar:universal_time_to_local_time(NowUniversalTime),

    {MegaSecs, Secs, _MicroSecs} = NowSecs,
    NowSeconds = MegaSecs * 1000000 + Secs,

    ets:insert(ets_timer, {now_seconds, NowSeconds}),
    ets:insert(ets_timer, {timer, {NowSecs, 0, NowUniversalTime, NowLocalTime}}),
    erlang:send_after(?CLOCK, self(), {event, clock}),

    set_last_daily_fresh(),
    erlang:send_after(?CLOCK_FOR_DAILY, self(), {event, clock_for_daily}),

    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({event, clock}, State) ->
    {_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
    NowSecs = erlang:now(),
    NowUniversalTime = calendar:now_to_universal_time(NowSecs),
    NowLocalTime = calendar:universal_time_to_local_time(NowUniversalTime),

    {MegaSecs, Secs, _MicroSecs} = NowSecs,
    NowSeconds = MegaSecs * 1000000 + Secs,

    ets:insert(ets_timer, {now_seconds, NowSeconds}),
    ets:insert(ets_timer, {timer, {NowSecs, Time_Since_Last_Call, NowUniversalTime, NowLocalTime}}),
    erlang:send_after(?CLOCK, self(), {event, clock}),
    {noreply, State};
handle_info({event, clock_for_daily}, State) ->
    erlang:send_after(?CLOCK_FOR_DAILY, self(), {event, clock_for_daily}),
    set_today_start(),
    set_last_daily_fresh(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    misc:delete_monitor_pid(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

set_today_start() ->
    Now = misc_timer:now_seconds_without_pro(),
    {_, Time} = util:seconds_to_localtime(Now),
    Seconds = calendar:time_to_seconds(Time),
    TodayStart = Now - Seconds,
    ets:insert(ets_timer, {today_start, TodayStart}).

set_last_daily_fresh() ->
    %% 取当天0点 - 1秒的时间戳
    Now = misc_timer:now_seconds_without_pro(),
    {_, Time} = util:seconds_to_localtime(Now),
    Seconds = calendar:time_to_seconds(Time),
    DailyFreshTime = (Now - Seconds) + ?DAILY_REFRESH_TIME,
    ets:insert(ets_timer, {last_daily_fresh, DailyFreshTime}).

