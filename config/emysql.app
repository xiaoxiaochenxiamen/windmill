%% Settings (defaults in include/emysql.hrl):
%% default_timeout (TIMEOUT = 8000)
%% lock_timeout (LOCK_TIMEOUT = 5000)

{application, emysql, [
    {description, "Emysql - Erlang MySQL driver"},
    {vsn, "0.4.1"},
    {modules, []},
    {mod, {emysql_app, []}},
    {registered, [emysql_conn_mgr, emysql_sup]},
    {applications, [kernel, stdlib]},
    {env, [
      {default_timeout, 500000},
      {conn_test_period, 28000}]}
]}.
