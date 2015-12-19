-module (util).
-include ("windmill.hrl").
-export ([
        term_to_string/1,
        lookup_ets/2,
        get_init_config/1,
        execute_sql/1,
        insert_buff/1,
        insert_ets/2,
        formated_timestamp/0,
        seconds_to_localtime/1,
        insert_ets_worker/1,
        delete_ets_worker/1,
        update_cur_buff/1,
        get_cur_buff/0,
        rand/2
        ]).

-define(DIFF_SECONDS_0000_1970, 62167219200).
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

lookup_ets(Table, Key) ->
    ets:lookup(Table, Key).

insert_ets(Table, Data) ->
    ets:insert(Table, Data).

get_init_config(Key) ->
    [{_, Config}] = lookup_ets(?ETS_INIT_CONFIG, Key),
    Config.

execute_sql(Query) ->
    emysql:execute(?WINDMILL_EMYSQL_POOL, Query).


insert_buff(Data) ->
    insert_ets(?ETS_BUFF, Data).

formated_timestamp() ->
    % {Date, Time}   = erlang:localtime(),
    {{YYYY,MM,DD},{Hour,Min,Sec}} = misc_timer:now_local_time(),
    FormatDate = io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w", [YYYY,MM,DD,Hour,Min,Sec]),
    lists:flatten(FormatDate).

seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

insert_ets_worker(Data) ->
    ets:insert(?ETS_WORKER, Data).

delete_ets_worker(Key) ->
    ets:delete(?ETS_WORKER, Key).

update_cur_buff(Table) ->
    ets:insert(?ETS_INIT_CONFIG, {cur_buff, Table}).
get_cur_buff() ->
    [{_, Buff}] = ets:lookup(?ETS_INIT_CONFIG, cur_buff),
    Buff.

rand(Min, Max) when Min > Max ->
    rand(Max, Min);

rand(Min, Max) when is_integer(Min), is_integer(Max) ->
    <<A:96>> = crypto:strong_rand_bytes(12),
    Min + A rem (Max - Min + 1);

rand(_, _)  ->
    0.
