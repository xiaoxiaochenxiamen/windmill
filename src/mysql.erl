-module (mysql).
-behaviour(gen_server).
-include ("windmill.hrl").

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-export([start_link/0,
        make_db/0
        ]).

-define (MAX_DB_WRITE, 200000).

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
    {ok, 0}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.

handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info({update, Stamp}, Status) ->
    update_mysql(Status, Stamp),
    {noreply, Stamp};

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

update_mysql(Min, Max) ->
    Match = [{{'$1','$2','$3'}, [{'>','$3',Min},{'=<','$3',Max}], [{{'$1','$2'}}]}],
    case ets:select(?ETS_BUFF, Match, ?MAX_DB_WRITE) of
    {Data, Continuation} ->
        write_data(Data),
        select_buff_continue(Continuation);
    _ ->
        ok
    end.

select_buff_continue('$end_of_table') ->
    ok;

select_buff_continue(Continuation) ->
    case ets:select(Continuation) of
    {Data, Continuation} ->
        write_data(Data),
        select_buff_continue(Continuation);
    _ ->
        ok
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
    check_time(mysql_log, Time3 - Time2),
    io:format("Update DB row: ~p, Spend: ~p (ms)~n",[length(Data), Time2-Time1]).

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

make_field_string([{Id, Value} | T], Res) ->
    IdStr = util:term_to_string(Id),
    ValueStr = util:term_to_string(Value),
    NewRes = ",("++IdStr++","++ValueStr++")"++Res,
    make_field_string(T, NewRes).

make_update_mysql(Value) ->
    Table = util:get_init_config(mysql_table),
    nake_query_update_table_field_value(Table, "id,value", Value).

make_log_mysql(Value) ->
    Table = util:get_init_config(mysql_log_table),
    % Field = util:get_init_config(mysql_log_field),
    "INSERT INTO "++Table++"(id,value)"++" VALUES"++Value.



load_mysql() ->
    Query = make_load_mysql_query(),
    io:format("load mysql start ...~n"),
    Time1 = misc_timer:now_milliseconds(),
    Data = select_mysql_table(Query),
    Time2 = misc_timer:now_milliseconds(),
    io:format("load mysql fnish !~n"),
    io:format("mysql data len: ~p,  time: ~p (ms)~n", [length(Data), Time2 - Time1]),
    ok = save_buff_data(Data).

save_buff_data([]) ->
    ok;
save_buff_data([[Id, Value] | T]) ->
    util:insert_buff({Id, Value, 0}),
    save_buff_data(T).


make_load_mysql_query() ->
    Table = util:get_init_config(mysql_table),
    Range = util:get_init_config(mysql_table_key_range),
    % Field = util:get_init_config(mysql_table_field),
    Where = make_sql_where_from_range(Range),
    "SELECT id,value FROM "++Table++" WHERE "++Where.


nake_query_update_table_field_value(Table, Field, Value) ->
    UpdateField = make_update_filed(Field),
    "INSERT INTO "++Table++"("++Field++") VALUES"++Value++" ON DUPLICATE KEY UPDATE "++UpdateField.

make_update_filed(Field) ->
    FieldList = string:tokens(Field, ","),
    List = [","++X++"=VALUES("++X++")" || X <- FieldList],
    [_ | Str] = lists:concat(List),
    Str.

make_sql_where_from_range(Range) ->
    make_sql_where_from_range(Range, []).

make_sql_where_from_range([{Cid1, Cid3, _}], Res) ->
    "(cid1="++Cid1++" and cid3="++Cid3++")"++Res;

make_sql_where_from_range([{Cid1, Cid3, _} | T], Res) ->
    make_sql_where_from_range(T, " or (cid1="++Cid1++" and cid3="++Cid3++")"++Res).

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


make_db() ->
    create_table(),
    make_table_data().

make_table_data() ->
    make_table_data(1000000).

make_table_data(2000000) ->
    ok;

make_table_data(Min) ->
    Time1 = misc_timer:now_milliseconds(),
    Data = make_data(Min+1, Min + 200000),
    Time2 = misc_timer:now_milliseconds(),
    Value = make_field_string_int_table(Data, ""),
    Time3 = misc_timer:now_milliseconds(),
    Query = nake_query_update_table_field_value("user", "id,value,cid1,cid3", Value),
    % Query =  "INSERT INTO user(id,value,cid1,cid3) VALUES"++Value,
    Time4 = misc_timer:now_milliseconds(),
    execute_sql(Query),
    Time5 = misc_timer:now_milliseconds(),
    io:format("data ~p, Value ~p, Query ~p, mysql ~p, all ~p~n",
        [Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time5 - Time1]),
    make_table_data(Min + 200000).

make_data(Min, Max) ->
    [{X, util:rand(101, 999)/100, util:rand(1, 100), util:rand(1, 100)} || X <- lists:seq(Min, Max)].

make_field_string_int_table([], [_ | Res]) ->
    Res;

make_field_string_int_table([{Id, Value, Cid1, Cid3} | T], Res)  ->
    IdStr = util:term_to_string(Id),
    ValueStr = util:term_to_string(Value),
    Cid1Str = util:term_to_string(Cid1),
    Cid3Str = util:term_to_string(Cid3),
    NewRes = ",("++IdStr++","++ValueStr++","++Cid1Str++","++Cid3Str++")"++Res,
    make_field_string_int_table(T, NewRes).

create_table() ->
    create_table_user(),
    create_table_user_log().

create_table_user() ->
    Sql = "CREATE TABLE user(id INT PRIMARY KEY, value Float, cid1 INT, cid3 INT)",
    execute_sql(Sql).

create_table_user_log() ->
    Sql = "CREATE TABLE user_log(log_id INT PRIMARY KEY AUTO_INCREMENT, id INT, value Float)",
    execute_sql(Sql).
