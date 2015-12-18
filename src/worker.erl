-module (worker).
-include("windmill.hrl").
-behaviour(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-export([start_link/1
        ]).

start_link(Data) ->
    gen_server:start_link(?MODULE, Data, []).

init(Data) ->
    process_flag(trap_exit, true),
    inets:start(),
    {ok, lists:sort(key_to_str(Data))}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    util:delete_ets_worker_pid(self()),
    {ok, Status}.

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.

handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info(run, Status) ->
    Cycle = util:get_init_config(http_send_cycle),
    erlang:send_after(Cycle*1000, self(), run),
    NewStatus = http_request(Status),
    {noreply, NewStatus};

handle_info(_Info, Status) ->
    {noreply, Status}.


check_value(Data, Result) ->
    check_value(Data, Result, []).


check_value([], _, Res) ->
    Res;

check_value(Data, [], Res) when length(Data) > length(Res) ->
    Res ++ Data;

check_value(Data, [], Res) ->
    Data ++ Res;

check_value([H | Data], [H | Result], Res) ->
    check_value(Data, Result, [H | Res]);

check_value([{Key, Old} | Data], [{Key, New} | Result], Res) when Old == New ->
    check_value(Data, Result, [{Key, New} | Res]);

check_value([{Key, _} | Data], [{Key, New} | Result], Res) ->
    util:insert_buff({Key, New}),
    check_value(Data, Result, [{Key, New} | Res]);

check_value([{Old, Value} | Data], [{New, _} | Result], Res) when Old > New ->
    check_value([{Old, Value} | Data], Result, Res);

check_value([H | Data], Result, Res) ->
    check_value(Data, Result, [H | Res]).

http_request(Data) ->
    Once = util:get_init_config(http_per_send),
    Time = misc_timer:now_milliseconds(),
    NewData = http_send(Data, Once, []),
    check_http_time(Time),
    lists:sort(check_value(Data, NewData)).

http_send([], _, Res) ->
    Res;

http_send(Data, Once, Res) ->
    {Send, T} = get_worker_data(Data, Once, []),
    Url = make_url(Send),
    case catch httpc:request(Url) of
    {ok,{{"HTTP/1.1",200,"OK"}, _, Body}} ->
        Result = decode_json_res(Body),
        http_send(T, Once, Result ++ Res);
    _ ->
        http_send(Data, Once, Res)
    end.


check_http_time(Time) ->
    HttpTime = misc_timer:now_milliseconds() - Time,
    ets:insert(?ETS_WORKER_PID, {self(), HttpTime}).



make_url(Data) ->
    UrlPrefix = util:get_init_config(http_url_prefix),
    Key = make_url_key(Data),
    UrlPrefix++Key.

make_url_key(Data) ->
    make_url_key(Data, []).

make_url_key([], []) ->
    [];

make_url_key([], [_, _, _, _, _ | Res]) ->
    Res;

make_url_key([{Key, _} | T], Res) ->
    make_url_key(T, "%2cJ_"++Key++Res).

decode_json_res(Param) ->
    case catch json:decode(Param) of
    {ok, Data, []} ->
        make_http_result(Data);
    _ ->
        []
    end.

make_http_result(Data) ->
    make_http_result(Data, []).

make_http_result([], Res) ->
    lists:sort(Res);

make_http_result([{obj, [{"id", <<_, _, Key/binary>>}, {"p" ,Value}, _]} | T], Res) ->
    make_http_result(T, [{binary_to_list(Key), binary_to_float(Value)} | Res]);

make_http_result([_ | T], Res) ->
    make_http_result(T, Res).

key_to_str(Data) ->
    [{integer_to_list(Key), Value} || {Key, Value} <- Data].

get_worker_data([], _, Res) ->
    {Res, []};

get_worker_data(Data, 0, Res) ->
    {Res, Data};

get_worker_data([H | T], Num, Res) ->
    get_worker_data(T, Num - 1, [H | Res]).
