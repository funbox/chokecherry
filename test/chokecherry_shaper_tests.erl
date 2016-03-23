-module(chokecherry_shaper_tests).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun chokecherry_shaper_100_puts_and_100_gets/1,
        fun chokecherry_shaper_100000_puts_and_100000_gets/1
    ]}.

start() ->
    application:ensure_all_started(chokecherry),
    timer:sleep(100),
    supervisor:terminate_child(chokecherry_sup, chokecherry_writer),
    ok.

stop(_) ->
    application:stop(chokecherry).

chokecherry_shaper_100_puts_and_100_gets(_) ->
    L1 = [{[], "log ~p", X} || X <- lists:seq(1, 100)],
    [chokecherry_shaper:put(Log) || Log <- L1],
    L2 = lists:reverse(get_all(undefined, [])),
    ?_assertEqual(L1, L2).

chokecherry_shaper_100000_puts_and_100000_gets(_) ->
    L1 = [{[], "log ~p", X} || X <- lists:seq(1, 100000)],
    pause(os:timestamp()),
    [chokecherry_shaper:put(Log) || Log <- L1],
    timer:sleep(1500),
    L2 = [{[], "log ~p", X} || X <- lists:seq(90001, 100000)],
    L3 = lists:reverse(get_all(undefined, [])),
    ?_assertEqual(L2, L3).

get_all(FirstMessage, Acc) ->
    case chokecherry_shaper:get() of
        ok ->
            Acc;
        Log ->
            get_all(false, [Log | Acc])
    end.

pause(LastTimestamp) ->
    {M, S, _} = Now = os:timestamp(),
    case LastTimestamp of
        {M, S, _} -> pause(Now);
        _ -> ok
    end.
