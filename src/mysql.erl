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
        ]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    crypto:start(),
    application:start(emysql),
    add_pool(),
    load_mysql(),
    put(last, misc_timer:now_milliseconds()),
    ets:insert(ets_time_log, {mysql_min, 0}),
    ets:insert(ets_time_log, {mysql_max, 0}),
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
    erlang:send_after(5*1000, self(), run),
    update_mysql_data(),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.

add_pool()->
    MysqlHost = util:get_init_config(mysql_host),
    MysqlPort = util:get_init_config(mysql_port),
    MysqlUserName = util:get_init_config(mysql_username),
    MysqlPassword = util:get_init_config(mysql_password),
    MysqlDatabase = util:get_init_config(mysql_database),
    emysql:add_pool(windmill_pool, [
        {size, 10},
        {user, MysqlUserName},
        {host, MysqlHost},
        {port, MysqlPort},
        {password, MysqlPassword},
        {database, MysqlDatabase},
        {encoding, utf8}
    ]).


update_mysql_data() ->
    Now = misc_timer:now_milliseconds(),
    Last = get(last),
    Math = [{{'$1','$2','$3'},[{'>','$3',10},{'>',20,'$3'}],[{{'$1','$2'}}]}],
    Res = ets:select(ets_buff, Math, 200000),
    update_mysql_data(Res).

update_mysql_data({Data, '$end_of_table'}) ->
    write_data(Data);

update_mysql_data({Data, Continue}) ->
    write_data(Data),
    Res = ets:select(Continue),
    update_mysql_data(Res);

update_mysql_data(_) ->
    ok.


write_data([]) ->
    ok;

write_data(Data) ->
    Value = make_field_string(Data, ""),
    Query = make_update_mysql(Value),
    QueryLog = make_log_mysql(Value),
    Time1 = misc_timer:now_milliseconds(),
    execute_sql(Query),
    Time2 = misc_timer:now_milliseconds(),
    execute_sql(QueryLog),
    TimeStr = util:formated_timestamp(),
    Time3 = Time2 - Time1,
    io:format("~p  Update DB row: ~p ! Spend: ~s (ms)~n", [TimeStr, length(Data), Time3])
    check_time(Time3).

check_time(Time) ->
    case ets:lookup(ets_time_log, mysql_max) of
    [{_, Max}] when Max >= Time ->
        skip;
    _ ->
        ets:insert(ets_time_log, {mysql_max, Time})
    end,
    case ets:lookup(ets_time_log, mysql_min) of
    [{_, Min}] when Min =< Time ->
        skip;
    _ ->
        ets:insert(ets_time_log, {mysql_min, Time})
    end.

make_field_string([], [_ | Res]) ->
    Res;

make_field_string([{Id, Value} | T], Res) ->
    ValueStr = util:term_to_string(Value),
    IdStr = util:term_to_string(Id),
    NewRes = ",("++IdStr++","++ValueStr++")"++Res,
    make_field_string(T, NewRes).

make_update_mysql(Value) ->
    Table = util:get_init_config(mysql_table),
    Field = util:get_init_config(mysql_table_field),
    UpdateField = make_update_filed(Field),
    "INSERT INTO "++Table++"("++Field++")"++" VALUES"++Value++" ON DUPLICATE KEY UPDATE "++UpdateField.


make_log_mysql(Value) ->
    Table = util:get_init_config(mysql_log_table),
    Field = util:get_init_config(mysql_log_field),
    "INSERT INTO "++Table++"("++Field++")"++" VALUES"++Value.

make_update_filed(Field) ->
    FieldList = string:tokens(Field, ","),
    List = [","++X++"=VALUES("++X++")" || X <- FieldList],
    [_ | Str] = lists:concat(List),
    Str.

load_mysql() ->
    Query = make_load_mysql_query(),
    Time1 = misc_timer:now_milliseconds(),
    Data = select_mysql_table(Query),
    Time2 = misc_timer:now_milliseconds(),
    io:format("load  total: ~p,  time: ~p (ms)~n", [length(Data), Time2 - Time1]),
    write_ets_buff(Data),
    make_ets_url(Data),
    distribute(Data).

make_ets_url(Data) ->
    make_ets_url(Data, "", 0, 0).

make_ets_url([], _, _, _) ->
    ok;

make_ets_url(Data, Url, Num, 1000) ->
    ets:insert(ets_url, {Num, Url}),
    make_ets_url(Data, "", Num+1, 0);

make_ets_url([[Id | _] | T], Url, Num, Pos) ->
    IdStr = util:term_to_string(Id),
    make_ets_url(T, "%2cJ_"++IdStr++Url, Num, Pos+1).







make_load_mysql_query() ->
    Table = util:get_init_config(mysql_table),
    Max = util:get_init_config(mysql_table_key_max),
    Min = util:get_init_config(mysql_table_key_min),
    Field = util:get_init_config(mysql_table_field),
    [Id | _] = string:tokens(Field,","),
    "SELECT "++Field++" FROM "++Table++" WHERE "++Id++" BETWEEN "++Min++" AND "++Max.


select_mysql_table(Query) ->
    case catch execute_sql(Query) of
    {result_packet, _, _, Result, _} when is_list(Result)->
        Result;
    Reason ->
        io:format("SQL error: ~p~n", [Reason]),
        []
    end.

execute_sql(Query) ->
    emysql:execute(windmill_pool, Query).


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

write_ets_buff([]) ->
    ok;

write_ets_buff([[Id, Value] | T]) ->
    ets:insert(ets_buff, {Id, Value, 0}),
    write_ets_buff(T).





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
