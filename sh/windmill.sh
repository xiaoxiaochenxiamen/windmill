#!/bin/bash

NODE_NAME=windmill

CONFIG_FILE=run_${NODE_NAME}
MARK_FILE="../logs/mark.log"
SMP=auto
ERL_PROCESSES=1024000
ERL_PORTS=65535
cd ../config

ARGS=
while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
    -port) PORT=$1; shift ;;
        --) break ;;
        *) ARGS="$ARGS $PARAM" ;;
    esac
done

live()
{
    echo "--------------------------------------------------------------------"
    echo ""
    echo "重要提示: 节点将会以交互式模式启动"
    echo ""
    echo "如果想退出该模式请输入 q()，然后回车"
    echo ""
    echo "--------------------------------------------------------------------"
    echo "任意键继续"
    read foo
    erl +P ${ERL_PROCESSES} \
        +t 2048576 \
        -smp ${SMP} \
        -env ERL_MAX_PORTS ${ERL_PORTS} \
        -pa ../ebin \
        -sname ${NODE_NAME} \
        -boot start_sasl \
        -config ${CONFIG_FILE} \
        -s windmill_app start_app
}


help()
{
    echo "--------------------------------------------------------------------"
    echo ""
    echo "管理命令:"
    echo " live             以交互方式启动节点"
    echo ""
    echo "命令行参数，      如: ./windmill.sh live"
    echo ""
    echo "--------------------------------------------------------------------"
}

case $ARGS in
    ' live') live;;
    *) help;;
esac

