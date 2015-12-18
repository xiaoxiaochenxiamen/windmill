set NODE_NAME=windmill
set CONFIG=run_windmill
set SMP=auto
set ERL_PROCESSES=1024000
cd ../config
erl  +P %ERL_PROCESSES% -smp %SMP% -env ERL_MAX_PORTS 65535 -pa ../ebin -sname %NODE_NAME% -config %CONFIG% -boot start_sasl -s  windmill_app start_windmill_app

pause