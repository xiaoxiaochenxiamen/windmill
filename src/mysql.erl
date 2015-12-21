-module (mysql).
-behaviour(gen_server).
-include ("windmill.hrl").

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-export([start_link/0
    % make/1
        ]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    crypto:start(),
    Res = application:start(?EMYSQL_APP),
    io:format("emysql start ~p~n", [Res]),
    Pool = add_pool(),
    io:format("emysql poll ~p~n", [Pool]),
    io:format("~p -- pool : ~p start ok!~n", [?EMYSQL_APP, ?WINDMILL_EMYSQL_POOL]),
    load_mysql(),
    {ok, []}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.


handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info(run, Status) ->
    Sec = util:get_init_config(mysql_write_cycle),
    erlang:send_after(Sec*1000, self(), run),
    write_mysql_data(),
    {noreply, Status};

handle_info(save, Status) ->
    BuffA = ets:tab2list(?ETS_BUFF_A),
    BuffB = ets:tab2list(?ETS_BUFF_B),
    BuffC = ets:tab2list(?ETS_BUFF_C),
    write_data(BuffA),
    write_data(BuffB),
    write_data(BuffC),
    io:format("save fnish ! update mysql row: ~p !~n", [length(BuffA)+length(BuffB)+length(BuffC)]),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.

add_pool()->
    MysqlHost = util:get_init_config(mysql_host),
    MysqlPort = util:get_init_config(mysql_port),
    MysqlUserName = util:get_init_config(mysql_username),
    MysqlPassword = util:get_init_config(mysql_password),
    MysqlDatabase = util:get_init_config(mysql_database),
    emysql:add_pool(?WINDMILL_EMYSQL_POOL, [
        {size, 10},
        {user, MysqlUserName},
        {host, MysqlHost},
        {port, MysqlPort},
        {password, MysqlPassword},
        {database, MysqlDatabase},
        {encoding, utf8}
    ]).


write_mysql_data() ->
    TimeStr = util:formated_timestamp(),
    Buff = util:get_next_buff(),
    case ets:info(Buff, size) of
    0 ->
        util:update_cur_buff(Buff),
        io:format("~p is nil ! time: ~s~n", [Buff, TimeStr]);
    Size ->
        Data = ets:tab2list(Buff),
        ets:delete_all_objects(Buff),
        write_data(Data),
        util:update_cur_buff(Buff),
        io:format("update mysql row: ~p ! time: ~s~n", [Size, TimeStr])
    end.

write_data([]) ->
    ok;

write_data(Data) ->
    Value = make_field_string(Data, ""),
    Query = make_update_mysql(Value),
    QueryLog = make_log_mysql(Value),
    Time1 = misc_timer:now_milliseconds(),
    emysql:execute(?WINDMILL_EMYSQL_POOL, Query),
    Time2 = misc_timer:now_milliseconds(),
    emysql:execute(?WINDMILL_EMYSQL_POOL, QueryLog),
    Time3 = misc_timer:now_milliseconds(),
    check_time(mysql, Time2 - Time1),
    check_time(mysql_log, Time3 - Time2).

check_time(Key, Time) ->
    case ets:lookup(?ETS_TIME_LOG, Key) of
    [{_, Min, Max}] ->
        if
        Min > Time ->
            ets:insert(?ETS_TIME_LOG, {Key, Time, Max});
        Time > Max ->
            ets:insert(?ETS_TIME_LOG, {Key, Min, Time});
        true ->
            skip
        end;
    _ ->
       ets:insert(?ETS_TIME_LOG, {Key, Time, Time})
    end.

make_field_string([], [_ | Res]) ->
    Res;

make_field_string([{Key, Value} | T], Res) ->
    ValueStr = util:term_to_string(Value),
    NewRes = ",("++Key++","++ValueStr++")"++Res,
    make_field_string(T, NewRes).

make_update_mysql(Value) ->
    Table = util:get_init_config(mysql_table),
    Field = util:get_init_config(mysql_table_field),
    UpdateField = make_update_filed(Field),
    ?MAKE_QUERY_UPDATE_TABLE_FIELD_VALUE(Table, Field, Value, UpdateField).

make_log_mysql(Value) ->
    Table = util:get_init_config(mysql_log_table),
    Field = util:get_init_config(mysql_log_field),
    ?MAKE_QUERY_REPLACE_TABLE_FIELD_VALUE(Table, Field, Value).

make_update_filed(Field) ->
    FieldList = string:tokens(Field, ","),
    List = [","++X++"=VALUES("++X++")" || X <- FieldList],
    [_ | Str] = lists:concat(List),
    Str.

load_mysql() ->
    try
        Query = make_load_mysql_query(),
        io:format("load mysql start ...~n"),
        Time1 = misc_timer:now_milliseconds(),
        Data = select_mysql_table(Query),
        Time2 = misc_timer:now_milliseconds(),
        io:format("load mysql fnish !~n"),
        io:format("mysql data len: ~p,  time: ~p (ms)~n", [length(Data), Time2 - Time1]),
        io:format("start distribute data and start worker process ...~n"),
        distribute(Data)
    catch
        _:Reason ->
            io:format("load mysql fail : ~p~n", [Reason])
    end.

make_load_mysql_query() ->
    Table = util:get_init_config(mysql_table),
    Key = util:get_init_config(mysql_table_key),
    MaxKey = util:get_init_config(mysql_table_key_max),
    MinKey = util:get_init_config(mysql_table_key_min),
    Field = util:get_init_config(mysql_table_field),
    ?MAKE_QUERY_SELECT_TABLE(Table, Field, Key, MinKey, MaxKey).

select_mysql_table(Query) ->
    case catch execute_sql(Query) of
    {result_packet, _, _, Result, _} when is_list(Result)->
        Result;
    Reason ->
        io:format("select ohther error: ~p~n", [Reason]),
        []
    end.

execute_sql(Query) ->
    emysql:execute(?WINDMILL_EMYSQL_POOL, Query).


distribute(Data) ->
    Worker = util:get_init_config(worker_process),
    Len = length(Data),
    Once = get_woker_once(Len, Worker),
    distribute(Data, Once).

distribute([], _) ->
    io:format("distribute fnish ! worker process : ~p~n", [ets:info(?ETS_WORKER_PID, size)]),
    io:format("start run worker process ...~n"),
    F = fun({Pid, _}, Msg) ->
            Pid ! Msg,
            Msg
        end,
    ets:foldl(F, run, ?ETS_WORKER_PID),
    Sec = util:get_init_config(mysql_write_cycle),
    erlang:send_after(Sec*1000, self(), run),
    io:format("all worker process start ok !~n"),
    io:format("start sync data ...~n"),
    ok;

distribute(Data, Once) ->
    {WorkerData, T} = get_worker_data(Data, Once, []),
    case catch supervisor:start_child(worker_sup, [WorkerData]) of
    {ok, Pid} ->
         util:insert_ets_worker_pid({Pid, 0}),
        distribute(T, Once);
    _ ->
        distribute(Data, Once)
    end.

get_woker_once(Len, Worker) ->
    case Len rem Worker of
    0 ->
        Len div Worker;
    _ ->
        Len div Worker + 1
    end.


get_worker_data([], _, Res) ->
    {Res, []};

get_worker_data(Data, 0, Res) ->
    {Res, Data};

get_worker_data([[Key, Value] | T], Num, Res) ->
    get_worker_data(T, Num - 1, [{Key, Value} | Res]).

% make(Float) ->
%     make(1000000, 1200000, Float),
%     make(1200000, 1400000, Float),
%     make(1400000, 1600000, Float),
%     make(1600000, 1800000, Float),
%     make(1800000, 2000000, Float).

% make(Min, Max, Float)  ->
%     Time1 = misc_timer:now_milliseconds(),
%     Data = make_data(Min, Max, Float) ,
%     Time2 = misc_timer:now_milliseconds(),
%     Value = make_field_string(Data, ""),
%     Time3 = misc_timer:now_milliseconds(),
%     Query = make_update_mysql(Value),
%     Time4 = misc_timer:now_milliseconds(),
%     emysql:execute(?WINDMILL_EMYSQL_POOL, Query),
%     Time5 = misc_timer:now_milliseconds(),
%     io:format("data ~p, Value ~p, Query ~p, mysql ~p, all ~p~n",
%         [Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time5 - Time1]).

% make_data(Min, Max, Float) ->
%     [{util:term_to_string(X), Float} || X <- lists:seq(Min, Max)].
