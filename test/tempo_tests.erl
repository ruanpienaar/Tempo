-module(tempo_tests).

-ifdef(EUNIT_DEBUG).
-define(NODEBUG, false).
-else.
-define(NODEBUG, true).
-endif.

-include_lib("eunit/include/eunit.hrl").

tempo_unit_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [

        {"start_test", fun start/0},
        {"limit_test", fun limit/0},
        {timeout, 120, [{"long_limit_test", fun long_limit/0}]}

     ]
    }.

start() ->
    ?debugFmt("TEST start() \n", []),
    ?assertMatch(
        P when is_pid(P),
        tempo:start_link(tbl, 10, 1000)
    ).

limit() ->
    ?debugFmt("TEST limit() \n", []),
    % Only allow 1 call per 10,000ms
    P = tempo:start_link(tbl, 1, 10000),
    ?assertEqual(
        true,
        tempo:can_make_call(P)
    ),
    % Check a few times
    timer:sleep(100),
    ?assertEqual(
        false,
        tempo:can_make_call(P)
    ),
    timer:sleep(100),
    ?assertEqual(
        false,
        tempo:can_make_call(P)
    ),
    timer:sleep(100),
    ?assertEqual(
        false,
        tempo:can_make_call(P)
    ),
    timer:sleep(100),
    ?assertEqual(
        false,
        tempo:can_make_call(P)
    ).

long_limit() ->
    ?debugFmt("TEST long_limit() \n", []),
    tbl = ets:new(tbl, [named_table, ordered_set, public]),
    % only 10 calls allowed after 10s
    P = tempo:start_link(tbl, 10, 10000),
    erlang:spawn(fun() -> fast_calling_loop(100000) end),
    erlang:send_after(9999, self(), time_up),
    receive
        time_up ->
            ok
    end,
    ?assertEqual(
        10,
        ets:info(tbl, size)
    ).



fast_calling_loop(Tbl, Pid, Count) when Count =< 0 ->
    ok;
fast_calling_loop(Tbl, Pid, Count) ->
    case tempo:can_make_call(P) of
        true ->
            true = ets:insert(tbl, {os:timestamp(), undefined});
        false ->
            ok
    end,
    fast_calling_loop(Tbl, Pid, Count-1).