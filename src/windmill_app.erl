-module (windmill_app).
-behaviour(application).
-export([start/2,
        stop/1,
        start_app/0
        ]).


start_app() ->
    application:start(windmill).

start(_, _) ->
    init_ets(),
    init_config(),
    windmill_sup:start_link().

stop(_) ->
    ok.

init_config() ->
    InitConfig = application:get_all_env(),
    insert_init_config(InitConfig).

insert_init_config([]) ->
    ok;

insert_init_config([{mysql_port, Number} | T]) when is_integer(Number), Number > 0 ->
    ets:insert(ets_init_config, {mysql_port, Number}),
    insert_init_config(T);

insert_init_config([{mysql_key_range, {Min, Max}} | T]) when is_integer(Min), Min >= 0, is_integer(Max), Max >= Min ->
    ets:insert(ets_init_config, {mysql_key_range, {Min, Max}}),
    insert_init_config(T);

insert_init_config([{_, Str} = H | T]) when is_list(Str) ->
    ets:insert(ets_init_config, H),
    insert_init_config(T);

insert_init_config([H | _]) ->
    io:format("bad config : ~p~n", [H]).




init_ets() ->
    ets:new(ets_time_log, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ets:new(ets_init_config, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ets:new(ets_url, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ets:new(ets_buff, [set, public, {keypos, 1}, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    ets:new(ets_worker, [set, public, {keypos, 1}, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    ok.
