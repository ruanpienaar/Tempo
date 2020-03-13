-module(tempo_tests).

-ifdef(EUNIT_DEBUG).
-define(NODEBUG, false).
-else.
-define(NODEBUG, true).
-endif.

-include_lib("eunit/include/eunit.hrl").

-export([
    low_limit_low_rate_demo/0,
    low_limit_high_rate_demo/0,
    low_limit_very_high_rate_demo/0,
    high_limit_low_rate_demo/0,
    high_limit_high_rate_demo/0
]).

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


%% DEMO's ( TODO - Move to eunit test )

%% Uncomment the above io:format's for more info when running the demo's.
% Send 'send_something' every 1000ms/1sec, rate limited to 1 per s,
% Should see only 10
low_limit_low_rate_demo() ->
    Pid = tempo:start_link(countme, 1, 1000),% 1 Per second
    SPid = spawn(fun() -> sender_loop(Pid, 0) end),
    {ok, _TRef} = timer:send_interval(1000, SPid, send_something),
    erlang:send_after(10000, SPid, done).

% sending 5 per second for 10 seconds
% Should see only 10
low_limit_high_rate_demo() ->
    Pid = tempo:start_link(countme, 1, 1000),% 1 Per second
    SPid = spawn(fun() -> sender_loop(Pid, 0) end),
    {ok, _TRef} = timer:send_interval(200, SPid, send_something),
    erlang:send_after(10000, SPid, done).

% sending 50 per second for 10 seconds
% Should see only 10
low_limit_very_high_rate_demo() ->
    Pid = tempo:start_link(countme, 1, 1000),% 1 Per second
    SPid = spawn(fun() -> sender_loop(Pid, 0) end),
    {ok, _TRef} = timer:send_interval(20, SPid, send_something),
    erlang:send_after(10000, SPid, done).

high_limit_low_rate_demo() ->
    Pid = tempo:start_link(countme, 100, 1000),
    SPid = spawn(fun() -> sender_loop(Pid, 0) end),
    {ok, _TRef} = timer:send_interval(20, SPid, send_something),
    erlang:send_after(10000, SPid, done).

high_limit_high_rate_demo() ->
    % run for 10 seconds, 100000 allowed per second. - Total 1 mil msgs.
    % Rather than using timer:send_after, just spawn a infinite_loop and kill it later.
    Pid = tempo:start_link(countme, 100000, 1000),
    IPid = spawn(fun() -> infinite_loop(Pid) end),
    erlang:send_after(10000, IPid, done).

% io:format(".~n", []) -> 34ms

% Ets time:
% Tid = ets:new(tbl, [public, set]).
% ets:insert(Tid, {foobar, 0}).
% lists:sum([ begin {I,_} = timer:tc(ets, update_counter, [Tid, foobar, 1]), I end || _X <- lists:seq(1, 1000000) ]) / 1000000.
% 0.572483

sender_loop(Pid, C) ->
    receive
        send_something ->
            Pid ! {self(), hit},
            NC =
                receive
                    limit ->
                        C;
                    ok ->
                        C+1
                end,
            sender_loop(Pid, NC);
        done ->
            io:format("Counted ~p sent~n",[C]),
            io:format("done load testing...~n"),
            io:format("remaining messages are from the mailbox in loop/6~n"),
            timer:sleep(5000),
            erlang:exit(Pid, kill),
            io:format("~nDONE~n")
    end.

infinite_loop(Pid) ->
    receive
        % Just keep trying :)
        R when R == ok orelse R == limit ->
            Pid ! {self(), hit},
            infinite_loop(Pid);
        done ->
            io:format("done load testing...~n"),
            io:format("remaining messages are from the mailbox in loop/6~n"),
            timer:sleep(5000),
            erlang:exit(Pid, kill)
    after
        0 ->
            Pid ! {self(), hit},
            infinite_loop(Pid)
    end.
