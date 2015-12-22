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

-define (UrlBody1, "&body=%7B%22catelogyIdLevel1%22%3A%22").
-define (UrlBody2, "%22%2C%22stock%22%3A%221%22%2C%22pagesize%22%3A%2210%22%2C%22catelogyId%22%3A%22").
-define (UrlBody3, "%22%2C%22page%22%3A%22").
-define (UrlBody4, "%22%2C%22cid%22%3A%22").
-define (UrlBody5, "%22%7D").

-record(worker,{
    id,
    init
    }).

start_link(Init) ->
    gen_server:start_link(?MODULE, Init, []).

init({Num, Init}) ->
    process_flag(trap_exit, true),
    inets:start(),
    insert_woker_pid(Num, Init),
    put(http_time, 0),
    {ok, #worker{
        id = Num,
        init = Init
    }}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    util:delete_ets_worker(Status#worker.id),
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
    windmill ! over;

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
    Url = util:get_init_config(http_url),
    PagerStr = util:term_to_string(Pager),
    Url++?UrlBody1++Cid1++?UrlBody2++Cid3++?UrlBody3++PagerStr++?UrlBody4++Cid3++?UrlBody5.


decode_json_res([]) ->
    [];

decode_json_res(Param) ->
    case catch json:decode(Param) of
    {ok, {obj, Data}, []} ->
        WareInfo = get_wareInfo(Data),
        make_http_res(WareInfo);
    A ->
        io:format("decode json ~p~n", [A]),
        []
    end.

get_wareInfo([]) ->
    [];

get_wareInfo([{"wareInfo", WareInfo} | _]) ->
    WareInfo;

get_wareInfo([_ | T]) ->
    get_wareInfo(T).

make_http_res(WareInfo) ->
    % io:format("wareinfo ~p~n", [WareInfo]),
    make_http_res(WareInfo, []).

make_http_res([], Res) ->
    Res;

make_http_res([{obj, Data} | T], Res) ->
    case get_id_value_bin(Data) of
    {Id, Value} ->
        make_http_res(T, [{Id, Value} | Res]);
    _ ->
        make_http_res(T, Res)
    end;

make_http_res([_ | T], Res) ->
    make_http_res(T, Res).

get_id_value_bin(Data) ->
    get_id_value_bin(Data, [], []).


get_id_value_bin(_, Id, Value) when Id =/= [], Value =/= [] ->
    {binary_to_integer(Id), binary_to_float(Value)};

get_id_value_bin([], _, _) ->
    [];

get_id_value_bin([{"wareId", Id} | T], _, Value) ->
     get_id_value_bin(T, Id, Value);

get_id_value_bin([{"jdPrice", Value} | T], Id, _) ->
     get_id_value_bin(T, Id, Value);

get_id_value_bin([_ | T], Id, Value) ->
     get_id_value_bin(T, Id, Value).


insert_woker_pid(Num, Data) ->
    ets:insert(?ETS_WORKER, {Num, self(), Data, 0}).

update_buff_data([], _) ->
    ok;

update_buff_data([{Id, Value} | T], Stamp) ->
    case ets:lookup(?ETS_BUFF, Id) of
    [{Id, Value, _}] ->
        skip;
    [{Id, Data, _}] when Data == Value ->
        skip;
    [{Id, _, _}] ->
        ets:insert(?ETS_BUFF, {Id, Value, Stamp});
    _ ->
        skip
    end,
    update_buff_data(T, Stamp).
