#!/bin/sh

# @copyright 2012 tvzavr.ru
# @author Ilya w-495 Nikitin
# @doc Erlang application example for automatically reloading modified modules
# during developing and production.
#

# ---------------------------------------------------------------------------
    NAME="webserver"
    COOKIE=mediesegmeatunetc
    CONFIG=$NAME
# ---------------------------------------------------------------------------
    ACTION=$1
    PATH=$PATH:/usr/local/lib/
    export PATH


    IP=`ifconfig  | grep 'inet addr:'| grep -v '127.0.0.1'  --max-count=1 | \
        cut -d: -f2 | awk '{print $1}'`

    if [ "" = "$IP" ]; then
        IP="localhost"
    fi


    MAIN_NODE="$NAME@$IP"
    MAIN_APP=$NAME"_app"

    CTRL_NODE="$NAME`date +_nodeclt_%H_%M_%S_%N`b@$IP"
    ERL_ARGS="+K true +A 128 +P 134217727"
    ERL_MAX_ETS_TABLES=140000
    export ERL_MAX_ETS_TABLES

    OTHERBINPATH=`find ./deps  -path '*.git' -prune -o  -type d -name 'ebin' -printf '%h/%f '`
    LOCALLBINPATH=`find ./ebin -type d -printf '%h/%f '`
    BINPATH=${OTHERBINPATH}" "${LOCALLBINPATH}
    SESSIONDBPATH='"./priv/session-db/'$MAIN_NODE'/"'
# ---------------------------------------------------------------------------
    if [ "" = "$ACTION"   ]; then
        echo "
        USAGE:
            $0  <command> [<arg> ...]
            MANAGE
                start [-detached]   - start up node
                startd              - alias to 'start -detached'
                reload_code         - hot code swap
                reload_cfg          - reloads config
                status              - request application status
                stop                - stops application and halts the node
                stop_app            - stops application, leaving the node running
                start_app           - starts application on an already-running node
                version             - request application version
        "
    else
        echo "
        INFO:
            MAIN_NODE:              $MAIN_NODE
            MAIN_APP:               $MAIN_APP
            CTRL_NODE:              $CTRL_NODE
            CONFIG:                 $CONFIG
            ERL_ARGS:               $ERL_ARGS
            ERL_MAX_ETS_TABLES:     $ERL_MAX_ETS_TABLES
            OTHERBINPATH:           $OTHERBINPATH
            LOCALLBINPATH:          $LOCALLBINPATH
            BINPATH:                $BINPATH
            SESSIONDBPATH:          $SESSIONDBPATH
        "

        if [ "start" = "$ACTION" ] || [ "startd" = "$ACTION" ] ; then
            if [ "startd" = "$ACTION" ] ; then
                ERL_ARGS=${ERL_ARGS}" -detached"
            fi
            shift
            rm -rf $SESSIONDBPATH
            erl \
                -pa $BINPATH \
                -boot start_sasl \
                -config ${CONFIG} \
                -name ${MAIN_NODE} \
                -setcookie ${COOKIE} \
                -s nodeclt_reloader \
                -s ${MAIN_APP} \
                -mnesia dir $SESSIONDBPATH \
                ${ERL_ARGS} \
            "$@"
        else
            erl \
                -noinput \
                -pa $BINPATH \
                -name ${CTRL_NODE} \
                -setcookie ${COOKIE} \
                -s nodeclt \
                -hidden \
                -connect_all false \
                ${ERL_ARGS} \
                -extra -n ${MAIN_NODE} \
            "$@"
        fi
    fi;
