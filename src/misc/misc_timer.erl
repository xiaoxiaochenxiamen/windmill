-module(misc_timer).

-export([
        now/0,
        now_seconds/0,
        now_universal_time/0,
        now_local_time/0,
        now_milliseconds/0,
        now_microsecs/0,
        longunixtime/0,
        unixtime/0,
        cpu_time/0,
        now_seconds_without_pro/0,
        now_milliseconds_without_pro/0,
        last_daily_fresh_time/0,
        get_today_start/0,
        seconds_to_date_num/1,
        seconds_to_localtime/1,
        check_update/1,
        check_update/2,
        get_date/0,
        week_day_of_now/0
    ]).

-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(DEFAULT_FRESH_TIME,             5).                         % 5点刷新
%% ====================================================================
%% External functions
%% ====================================================================
now() ->
    erlang:now().

now_seconds_without_pro() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    MegaSecs * 1000000 + Secs.

now_milliseconds_without_pro() ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    NowMicroSecs = MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs,
    NowMicroSecs div 1000.

now_seconds()->
    [{now_seconds, NowSeconds}] = ets:lookup(ets_timer, now_seconds),
    NowSeconds.

now_universal_time()    ->
    [{timer, {_, _, NowUniversalTime, _}}] = ets:lookup(ets_timer, timer),
    NowUniversalTime.

now_local_time()    ->
    [{timer, {_, _, _, NowLocalTime}}] = ets:lookup(ets_timer, timer),
    NowLocalTime.

%% 毫秒
now_milliseconds() ->
    (now_microsecs()) div 1000.

%% 微秒
now_microsecs() ->
    [{timer, {Now, _, _, _}}] = ets:lookup(ets_timer, timer),
    {M, S, Ms} = Now,
    M * 1000000000000 + S * 1000000 + Ms.

cpu_time() ->
    {_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
    Time_Since_Last_Call.

longunixtime() ->
    now_milliseconds().

unixtime() ->
    now_seconds().

last_daily_fresh_time() ->
    [{last_daily_fresh, Time}] = ets:lookup(ets_timer, last_daily_fresh),
    Time.

get_today_start() ->
    [{today_start, TodayStart}] = ets:lookup(ets_timer, today_start),
    TodayStart.

get_date() ->
    [{timer, {_, _, _, {Date, _}}}] = ets:lookup(ets_timer, timer),
    Date.

%% -----------------------------------------------------------------
%% 根据1970年以来的秒数获得日期
%% -----------------------------------------------------------------
%% misc:seconds_to_localtime(misc:seconds()).
seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).
%% 根据秒数获得日期 --20120630
seconds_to_date_num(Seconds) ->
    {{Y, M, D}, {_, _, _}} = seconds_to_localtime(Seconds),
    Y * 10000 + M * 100 + D.

%% @param time上次更新时间戳
%% @return true应该更新|false不更新
check_update(Time) ->
    {{Y1, M1, D1}, {H1, _, _}} = misc_timer:seconds_to_localtime(now_seconds()),
    {{Y2, M2, D2}, {H2, _, _}} = misc_timer:seconds_to_localtime(Time),
    if H2 < ?DEFAULT_FRESH_TIME ->
           if H1 >= ?DEFAULT_FRESH_TIME ->
                  true;
              D1 > D2 orelse M1 > M2 orelse Y1 > Y2 ->
                  true;
              true ->
                  false
           end;
       true ->
           if D1 =:= D2 andalso M1 =:= M2 andalso Y1 =:= Y2 ->
                  false;
              D1 =:= (D2+1) andalso H1 < ?DEFAULT_FRESH_TIME ->
                  false;
              true ->
                  true
           end
    end.
%% @param time上次更新时间戳,Hour每日更新的小时数
%% @return true应该更新|false不更新
check_update(Time, Hour) ->
    {{Y1, M1, D1}, {H1, _, _}} = misc_timer:seconds_to_localtime(now_seconds()),
    {{Y2, M2, D2}, {H2, _, _}} = misc_timer:seconds_to_localtime(Time),
    if H2 < Hour ->
           if H1 >= Hour ->
                  true;
              D1 > D2 orelse M1 > M2 orelse Y1 > Y2 ->
                  true;
              true ->
                  false
           end;
       true ->
           if D1 =:= D2 andalso M1 =:= M2 andalso Y1 =:= Y2 ->
                  false;
              D1 =:= (D2+1) andalso H1 < Hour ->
                  false;
              true ->
                  true
           end
    end.

%% 返回当前周几,周一返回1,周二返回2,...
week_day_of_now() ->
    {{Y, M, D}, {_, _, _}} = misc_timer:seconds_to_localtime(now_seconds()),
    calendar:day_of_the_week(Y, M, D).

