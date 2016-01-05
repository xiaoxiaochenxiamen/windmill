-module (util).
-export ([
        term_to_string/1,
        get_init_config/1,
        formated_timestamp/0,
        seconds_to_localtime/1,
        insert_ets_worker_pid/1,
        delete_ets_worker_pid/1,
        update_cur_buff/1,
        get_cur_buff/0
        ]).

-define(DIFF_SECONDS_0000_1970, 62167219200).

term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

get_init_config(Key) ->
    [{_, Config}] = ets:lookup(ets_init_config, Key),
    Config.


formated_timestamp() ->
    % {Date, Time}   = erlang:localtime(),
    {{YYYY,MM,DD},{Hour,Min,Sec}} = misc_timer:now_local_time(),
    FormatDate = io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w", [YYYY,MM,DD,Hour,Min,Sec]),
    lists:flatten(FormatDate).

seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

insert_ets_worker_pid(Data) ->
    ets:insert(ets_worker, Data).

delete_ets_worker_pid(Key) ->
    ets:delete(ets_worker, Key).

update_cur_buff(Table) ->
    ets:insert(ets_init_config, {cur_buff, Table}).
get_cur_buff() ->
    [{_, Buff}] = ets:lookup(ets_init_config, cur_buff),
    Buff.
