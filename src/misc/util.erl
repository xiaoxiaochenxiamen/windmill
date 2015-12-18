-module (util).
-include ("windmill.hrl").
-export ([
        term_to_string/1,
        get_ets_value/2,
        get_init_config/1,
        execute_sql/1,
        insert_buff/1,
        formated_timestamp/0,
        seconds_to_localtime/1,
        insert_ets_worker_pid/1,
        delete_ets_worker_pid/1,
        get_next_buff/0,
        update_cur_buff/1,
        get_cur_buff/0
        ]).

-define(DIFF_SECONDS_0000_1970, 62167219200).
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

get_ets_value(Table, Key) ->
    ets:lookup(Table, Key).

insert_ets(Table, Data) ->
    ets:insert(Table, Data).

get_init_config(Key) ->
    [{_, Config}] = get_ets_value(?ETS_INIT_CONFIG, Key),
    Config.

execute_sql(Query) ->
    emysql:execute(?WINDMILL_EMYSQL_POOL, Query).


insert_buff(Data) ->
    Table = get_init_config(cur_buff),
    insert_ets(Table, Data).

formated_timestamp() ->
    % {Date, Time}   = erlang:localtime(),
    {{YYYY,MM,DD},{Hour,Min,Sec}} = misc_timer:now_local_time(),
    FormatDate = io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w", [YYYY,MM,DD,Hour,Min,Sec]),
    lists:flatten(FormatDate).

seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

insert_ets_worker_pid(Data) ->
    ets:insert(?ETS_WORKER_PID, Data).

delete_ets_worker_pid(Key) ->
    ets:delete(?ETS_WORKER_PID, Key).

get_next_buff() ->
    case get_init_config(cur_buff) of
    ?ETS_BUFF_A ->
        ?ETS_BUFF_B;
    ?ETS_BUFF_B ->
        ?ETS_BUFF_C;
    _ ->
        ?ETS_BUFF_A
    end.
update_cur_buff(Table) ->
    ets:insert(?ETS_INIT_CONFIG, {cur_buff, Table}).
get_cur_buff() ->
    [{_, Buff}] = ets:lookup(?ETS_INIT_CONFIG, cur_buff),
    Buff. 