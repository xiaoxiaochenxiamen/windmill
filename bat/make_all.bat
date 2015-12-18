echo

set ROOT_DIR=%~dp0..\

del /q %ROOT_DIR%ebin

mkdir %ROOT_DIR%ebin

cd %ROOT_DIR%\src

erl -pa ../ebin -pa ../emysql/ebin -make

cd %ROOT_DIR%\bat

pause