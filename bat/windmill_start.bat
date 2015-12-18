set NODE_NAME=windmill
set CONFIG=windmill
set SMP=auto
set ERL_PROCESSES=1024000
cd ../config
erl  +P %ERL_PROCESSES% \
     -smp %SMP% \
     -env ERL_MAX_PORTS 65535 \
     -pa ../ebin ../Emysql/ebin \
    -sname %NODE_NAME% \
    -config %CONFIG% \
    -boot start_sasl \
    -s  windmill_app start_app

pause
