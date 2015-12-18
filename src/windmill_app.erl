-module (windmill_app).
-behaviour(application).
-include ("windmill.hrl").
-export([start/2,
        stop/1,
        start_app/0
        ]).


start_app() ->
    application:start(windmill).

start(_, _) ->
    try
        init_ets(),
        init_config(),
        windmill_sup:start_link()
    catch
        _:Reason ->
            io:format("windmill app start fail, ~p~n", [Reason])
    end.

stop(_) ->
    io:format("windmill stop...~n"),
    ok.

init_config() ->
    InitConfig = application:get_all_env(),
    insert_init_config(InitConfig),
    ets:insert(?ETS_INIT_CONFIG, {cur_buff, ?ETS_BUFF_A}).

insert_init_config([]) ->
    ok;

insert_init_config([{mysql_port, Number} | T]) when is_integer(Number), Number > 0 ->
    ets:insert(?ETS_INIT_CONFIG, {mysql_port, Number}),
    insert_init_config(T);

insert_init_config([{mysql_per_write, Number} | T]) when is_integer(Number), Number > 0 ->
    ets:insert(?ETS_INIT_CONFIG, {mysql_per_write, Number}),
    insert_init_config(T);

insert_init_config([{worker_process, Number} | T]) when is_integer(Number), Number > 0 ->
    ets:insert(?ETS_INIT_CONFIG, {worker_process, Number}),
    insert_init_config(T);

insert_init_config([{http_url_prefix, {_,_,_,_} = Prefxi} | T]) ->
    ets:insert(?ETS_INIT_CONFIG, {http_url_prefix, Prefxi}),
    insert_init_config(T);

insert_init_config([{mysql_write_cycle, Number} | T]) when is_integer(Number), Number > 0 ->
    ets:insert(?ETS_INIT_CONFIG, {mysql_write_cycle, Number}),
    insert_init_config(T);

insert_init_config([{http_send_cycle, Number} | T]) when is_integer(Number), Number > 0 ->
    ets:insert(?ETS_INIT_CONFIG, {http_send_cycle, Number}),
    insert_init_config(T);

insert_init_config([{mysql_table_key_range, Range} | T]) when is_list(Range), Range /= "" ->
    case checkRange(Range) of
    true ->
        ets:insert(?ETS_INIT_CONFIG, {mysql_table_key_range, Range}),
        insert_init_config(T);
    _ ->
        io:format("bad config : ~p~n", [{mysql_table_key_range, Range}])
    end;

insert_init_config([{_, Str} = H | T]) when is_list(Str) ->
    ets:insert(?ETS_INIT_CONFIG, H),
    insert_init_config(T);

insert_init_config([H | _]) ->
    io:format("bad config : ~p~n", [H]).

checkRange([]) ->
    true;

checkRange([{Cid1, Cid2, Pager} | T ]) when is_list(Cid1), is_list(Cid2), is_list(Pager) ->
    checkRange(T);

checkRange(_) ->
    false.


init_ets() ->
    ets:new(?ETS_TIME_LOG, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ets:new(?ETS_INIT_CONFIG, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ets:new(?ETS_WORKER_PID, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ets:new(?ETS_BUFF_A, [set, public, {keypos, 1}, named_table, {write_concurrency, true}]),
    ets:new(?ETS_BUFF_B, [set, public, {keypos, 1}, named_table, {write_concurrency, true}]),
    ets:new(?ETS_BUFF_C, [set, public, {keypos, 1}, named_table, {write_concurrency, true}]),
    ok.
