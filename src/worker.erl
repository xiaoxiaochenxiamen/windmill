-module (worker).
-behaviour(gen_server).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-export([start_link/1
        ]).

start_link(Init) ->
    gen_server:start_link(?MODULE, Init, []).

init(Init) ->
    process_flag(trap_exit, true),
    inets:start(),
    {ok, Init}.

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
    http_request(Status),
    erlang:send_after(1000, self(), run),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.

http_request({Min, Max}) ->
    http_request(Min, Max).

http_request(Min, Max) when Min > Max ->
    ok;

http_request(Min, Max) ->
    Time = misc_timer:now_milliseconds(),
    Body = http_send(Min),
    check_http_time(Time),
    Data = decode_json_res(Body),
    check_value(Data),
    http_request(Min + 1, Max).


http_send(Id) ->
    case ets:lookup(ets_url, Id) of
    [{Id, Url}] ->
        request_http_server(Url);
    _ ->
        []
    end.

request_http_server(Idstr) ->
    Url = make_url(Idstr),
    case catch httpc:request(Url) of
    {ok,{{"HTTP/1.1",200,"OK"}, _, Body}} ->
        io:format("~p~n", [Body]),
        Body;
    A ->
        io:format("~p~n", [A]),
        []
    end.


check_http_time(Time) ->
    HttpTime = misc_timer:now_milliseconds() - Time,
    ets:insert(ets_worker, {self(), HttpTime}).



make_url(Idstr) ->
    UrlPrefix = util:get_init_config(http_url_prefix),
    UrlPrefix++Idstr.

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
    Res;

make_http_result([{obj, [{"id", <<_, _, Key/binary>>}, {"p" ,Value}, _]} | T], Res) ->
    make_http_result(T, [{binary_to_integer(Key), binary_to_float(Value)} | Res]);

make_http_result([_ | T], Res) ->
    make_http_result(T, Res).

check_value([]) ->
    ok;

check_value([{Id, New} | T]) ->
    case ets:lookup(ets_buff, Id) of
    [{Id, Old, _}] when New /= Old ->
        ets:insert(ets_buff, {Id, New, misc_timer:now_milliseconds()});
    _ ->
        skip
    end,
    check_value(T).
