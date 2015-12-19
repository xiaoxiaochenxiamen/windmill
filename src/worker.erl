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

-record(worker,{
    id,
    init
    }).

start_link([Num, Init]) ->
    gen_server:start_link(?MODULE, [Num, Init], []).

init([Num, Init]) ->
    process_flag(trap_exit, true),
    inets:start(),
    insert_woker_pid(Num, Init),
    {ok, #worker{
        id = Num,
        init = Init
    }}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    util:delete_ets_worker_pid(Status#worker.id),
    {ok, Status}.

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.

handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info({run, Stamp}, Status) ->
    http_request(Status#worker.init, Stamp),
    update_log_http_time(Status#worker.id),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.


http_request({Cid1, Cid3, Pager}, Stamp) ->
    http_request_sync_data(Cid1, Cid3, Pager, Stamp).

http_request_sync_data(_Cid1, _Cid3, 0, _Stamp) ->
    ok;

http_request_sync_data(Cid1, Cid3, Pager, Stamp) ->
    Url = make_url(Cid1, Cid3, Pager),
    Time = misc_timer:now_milliseconds(),
    Body = post_request_to_sever(Url),
    check_http_time(Time),
    Data = decode_json_res(Body),
    update_buff_data(Data, Stamp),
    http_request_sync_data(Cid1, Cid3, Pager-1, Stamp).



post_request_to_sever(Url) ->
    case catch httpc:request(Url) of
    {ok,{{"HTTP/1.1",200,"OK"}, _, Body}} ->
        Body;
    _ ->
       []
    end.


check_http_time(Time) ->
    New = misc_timer:now_milliseconds() - Time,
    Old = get(http_time),
    case Old < New of
    true ->
        put(http_time, New);
    _ ->
        skip
    end.

update_log_http_time(Id) ->
    New = get(http_time),
    [{Id, Pid, Data, Old}] = ets:lookup(?ETS_WORKER, Id),
    case Old < New of
    true ->
        ets:insert(?ETS_WORKER, {Id, Pid, Data, New});
    _ ->
        skip
    end.

make_url(Cid1, Cid3, Pager) ->
    {Url1, Url2, Url3, Url4, Url5} = util:get_init_config(http_url_prefix),
    PagerStr = util:term_to_string(Pager),
    Url1++Cid1++Url2++Cid3++Url3++PagerStr++Url4++Cid3++Url5.


decode_json_res([]) ->
    [];

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

insert_woker_pid(Num, Data) ->
    ets:insert(?ETS_WORKER, {Num, self(), Data, 0}).

update_buff_data([], _) ->
    ok;

update_buff_data([{Id, Value} | T], Stamp) ->
    case ets:lookup(?ETS_BUFF, Id) of
    [{Id, Value, _}] ->
        skip;
    _ ->
        ets:insert(?ETS_BUFF, {Id, Value, Stamp})
    end,
    update_buff_data(T, Stamp).
