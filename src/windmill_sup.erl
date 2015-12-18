-module(windmill_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    io:format("windmill_app_sup:start ok!~n"),
    {ok, Pid}.


init(_) ->
    {ok,
        {{one_for_one, 10, 10},
            [
                {
                    mod_server_time,
                    {mod_server_time, start_link, []},
                    permanent,
                    brutal_kill,
                    worker,
                    [mod_server_time]
                },
                                {
                    worker_sup,
                    {worker_sup, start_link, []},
                    permanent,
                    brutal_kill,
                    supervisor,
                    [worker_sup]
                },
                {
                    mysql,
                    {mysql, start_link, []},
                    permanent,
                    infinity,
                    worker,
                    [mysql]
                },
                {
                    windmill,
                    {windmill, start_link, []},
                    permanent,
                    brutal_kill,
                    worker,
                    [windmill]
                }
            ]
        }
    }.
