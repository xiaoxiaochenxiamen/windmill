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
        peek/0
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    start_worker(),
    erlang:send_after(0, self(), worker_run),
    put(over, 0),
    {ok, []}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.

handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info(worker_run, Status) ->
    Now = misc_timer:now_seconds(),
    put(last, Now),
    worker_run(Now),
    {noreply, Status};

handle_info(over, Status) ->
    recive_worker_over(),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.


recive_worker_over() ->
    Over = get(over),
    Worker = get(worker),
    case Over + 1 == Worker of
    true ->
        Now = misc_timer:now_seconds(),
        Cycle = util:get_init_config(http_send_cycle),
        Last = get(last),
        Sec = Cycle - (Now - Last),
        erlang:send_after(Sec*1000, self(), worker_run),
        mysql ! {update, Last},
        put(over, 0);
    _ ->
        put(over, Over + 1)
    end.

peek() ->
    NowStr = util:formated_timestamp(),
    {MinHttp, MaxHttp} = get_http_time_log(),
    {MinMysql, MaxMysql} = get_mysql_time_log(mysql),
    {MinMyLog, MaxMyLog} = get_mysql_time_log(mysql_log),
    WorkerNumber = ets:info(?ETS_WORKER, size),
    BuffSize = ets:info(?ETS_BUFF, size),
    io:format("peek time : ~s~n", [NowStr]),
    io:format("worker process : ~p~n", [WorkerNumber]),
    io:format("data buff size : ~p ~n", [BuffSize]),
    io:format("HTTP  -- Min : ~p , Max : ~p (ms)~n", [MinHttp, MaxHttp]),
    io:format("Mysql -- Min : ~p , Max : ~p (ms)~n", [MinMysql, MaxMysql]),
    io:format("Mylog -- Min : ~p , Max : ~p (ms)~n", [MinMyLog, MaxMyLog]).


get_http_time_log() ->
    F = fun({_, _, _, Time}, {Min, Max}) ->
            if
            Time > Max ->
                {Min, Time};
            Time < Min ->
                {Time, Max};
            true ->
                {Min, Max}
            end
        end,
    case ets:foldl(F, {nil, -1}, ?ETS_WORKER) of
    {nil, X} ->
        {X, X};
    Other ->
        Other
    end.

get_mysql_time_log(Key) ->
    case ets:lookup(?ETS_TIME_LOG, Key) of
    [{_, Min, Max}] ->
        {Min, Max};
    _ ->
        {-1, -1}
    end.

start_worker() ->
    distribute(),
    io:format("distribute fnish ! worker process : ~p~n", [ets:info(?ETS_WORKER, size)]),
    io:format("start run worker process ...~n").

worker_run(Now) ->
    post_worker_msg({run, Now}),
    Tstr = util:formated_timestamp(),
    io:format("~p , start sync server data ...~n", [Tstr]).



distribute()->
    Range = util:get_init_config(mysql_table_key_range),
    distribute(Range, 0).

distribute([], N) ->
    put(worker, N);

distribute([H | T] = Data, N) ->
    case catch supervisor:start_child(worker_sup, [{N, H}]) of
    {ok, _} ->
        distribute(T, N+1);
    _ ->
        distribute(Data, N)
    end.

post_worker_msg(Msg) ->
    F = fun({_, Pid, _, _}, M) ->
            Pid ! M,
            M
        end,
    ets:foldl(F, Msg, ?ETS_WORKER).
