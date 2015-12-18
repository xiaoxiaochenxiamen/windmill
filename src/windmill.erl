-module (windmill).
-include("windmill.hrl").
-behaviour(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-export([start_link/0,
        peek/0,
        save/0
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, []}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.

handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info(_Info, Status) ->
    {noreply, Status}.


peek() ->
    NowStr = util:formated_timestamp(),
    {MinHttp, MaxHttp} = get_http_time_log(),
    {MinMysql, MaxMysql} = get_mysql_time_log(mysql),
    {MinMyLog, MaxMyLog} = get_mysql_time_log(mysql_log),
    WorkerNumber = ets:info(?ETS_WORKER_PID, size),
    BuffSize = ets:info(?ETS_BUFF_A, size) + ets:info(?ETS_BUFF_B, size) + ets:info(?ETS_BUFF_C, size),
    io:format("peek time : ~s~n", [NowStr]),
    io:format("worker process : ~p~n", [WorkerNumber]),
    io:format("data buff size : ~p ~n", [BuffSize]),
    io:format("HTTP  -- Min : ~p , Max : ~p (ms)~n", [MinHttp, MaxHttp]),
    io:format("Mysql -- Min : ~p , Max : ~p (ms)~n", [MinMysql, MaxMysql]),
    io:format("Mylog -- Min : ~p , Max : ~p (ms)~n", [MinMyLog, MaxMyLog]).

save() ->
    io:format("save buff  ...~n"),
    mysql ! save.
get_http_time_log() ->
    F = fun({_, Time}, {Min, Max}) ->
            if
            Time > Max ->
                {Min, Time};
            Time < Min ->
                {Time, Max};
            true ->
                {Min, Max}
            end
        end,
    ets:foldl(F, {nil, -1}, ?ETS_WORKER_PID).

get_mysql_time_log(Key) ->
    case ets:lookup(?ETS_TIME_LOG, Key) of
    [{_, Min, Max}] ->
        {Min, Max};
    _ ->
        {nil, -1}
    end.
