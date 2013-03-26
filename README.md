# node controller (Что это)

Простой динамический перезагрузчик кода для erlang в горячем режиме.
Стандартный набор ф-ий для управления нодой и приложением.
Собрано из кусков разных приложений.
Возможно не всегда корректно работает в текущем виде.
Пока проблем замечено не было.

# Requirements (Что нужно)

* gnumake
* erlang
* rebar

# Example (Пример)

Пример можно найти в ./priv/example-ctl.sh

## Для горячей замене по make

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


## Для горячей замене по ./<ctl> reload_code

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
    
### Параметры
* `start [-detached]`   — запусткает ноду;
* `startd`              — запусткает ноду в фоне, псевдоним к 'start -detached';
* `reload_code`         — заменяет код;
* `reload_cfg`          — заменяет конфигурацию нодыы;
* `status`              — возвращает статус приложения;
* `stop`                — останавливает приложение и ноду;
* `stop_app`            — останавливает только приложение;
* `start_app`           — запускает приложение поверх запущенной ноды;
* `version`             — возвращает версию приложения.

# Credis (Кто это натворил)

* Сергей Кожевников (Serge Kozhevnikov aka cff, 2011);
* Илья w-495 Никитин (w-495, 2013).
* В отлельных модулях указаны авторы, чьи варианты были взяты за основу.

# TODO
    * Документировать код.
    * Убрать лишнее и привести в нормальный вид.