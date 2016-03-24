-module(chokecherry_shaper_SUITE).

-export([all/0,
         group/1,
         groups/0,

         init_per_group/2,
         end_per_group/2,

         init_per_testcase/2,
         end_per_testcase/2]).

-export([check_that_100_in_100_out/1,
         check_that_10000_in_10000_out/1,
         check_that_100000_in_10000_out/1,
         check_strict_message_order/1]).

-include("chokecherry.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, main}].

group(main) ->
    [{timetrap, {seconds, 120}}].

groups() ->
    [
     {main,
      [shuffle, sequence],
      [
        check_that_100_in_100_out,
        check_that_10000_in_10000_out,
        check_that_100000_in_10000_out,
        check_strict_message_order
     ]
    }
   ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_, Config) ->
    ok = chokecherry:start(),
    ok = supervisor:terminate_child(chokecherry_sup, chokecherry_writer),
    Config.

end_per_group(_, _Config) ->
    ok = chokecherry:stop(),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

check_that_100_in_100_out(_Config) ->
    N = 100,
    L1 = [{[], "log ~p", X} || X <- lists:seq(1, N)],

    pause(os:timestamp()),

    [chokecherry_shaper:put(Log) || Log <- L1],
    ?assertEqual(N, length(poll())),
    ok.

check_that_10000_in_10000_out(_Config) ->
    N = 10000,
    L1 = [{[], "log ~p", X} || X <- lists:seq(1, N)],

    pause(os:timestamp()),

    [chokecherry_shaper:put(Log) || Log <- L1],
    ?assertEqual(N, length(poll())),
    ok.

check_that_100000_in_10000_out(_Config) ->
    N = 100000,
    L1 = [{[], "log ~p", X} || X <- lists:seq(1, N)],

    pause(os:timestamp()),

    [chokecherry_shaper:put(Log) || Log <- L1],
    ?assertEqual(N div 10, length(poll())),
    ok.

check_strict_message_order(_Config) ->
    N = 100000,
    L1 = [{[], "log ~p", X} || X <- lists:seq(1, N)],

    pause(os:timestamp()),

    [chokecherry_shaper:put(Log) || Log <- L1],
    L2 = [{[], "log ~p", X} || X <- lists:seq(90001, N)],
    L3 = poll(),
    ?assertEqual(lists:sort(L2), lists:sort(L3)),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

pause(LastTimestamp) ->
    {M, S, _} = Now = os:timestamp(),
    case LastTimestamp of
        {M, S, _} ->
            pause(Now);
        _ ->
            ok
    end.

poll() ->
    poll([]).

poll(Acc) ->
    case chokecherry_shaper:get() of
        ok ->
            Acc;
        Log ->
            poll([Log | Acc])
    end.

