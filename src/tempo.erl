-module(tempo).

-export([
    start_link/3,
    init/3
]).

-export([
    low_rate_demo/0,
    fast_rate_demo/0,
    very_fast_rate_demo/0
]).

start_link(LimitRef, Limit, Interval) ->
    proc_lib:start_link(?MODULE, init, [LimitRef, Limit, Interval]).

init(LimitRef, Limit, Interval) ->
    Tid = ets:new(LimitRef, [set, private]),
    true = ets:insert(Tid, {LimitRef, 0}),
    {ok, TRef} = timer:send_interval(Interval, cleanup),
    loop_init(Tid, LimitRef, Limit, TRef, Interval).

loop_init(Tid, LimitRef, Limit, TRef, Interval) ->
    proc_lib:init_ack(self()),
    loop(Tid, LimitRef, Limit, TRef, Interval).

loop(Tid, LimitRef, Limit, TRef, Interval) ->
    receive
        cleanup ->
            % io:format("C~p~n", [erlang:monotonic_time(nano_seconds)]),
            true = ets:insert(Tid, {LimitRef, 0}),
            loop(Tid, LimitRef, Limit, TRef, Interval);
        {ReqPid, hit} ->
            case ets:update_counter(Tid, LimitRef, 1) of
                % Plus 1 here, cause we already updated, and did not
                % lookup before we updated...
                NewCount when NewCount >= Limit + 1 ->
                    % io:format("LC ~p~n", [NewCount]),
                    % io:format("L ~p~n", [erlang:monotonic_time(nano_seconds)]),
                    ReqPid ! limit;
                NewCount ->
                    % io:format("HC ~p~n", [NewCount]),
                    % io:format("H ~p~n", [erlang:monotonic_time(nano_seconds)]),
                    ReqPid ! ok
            end,
            loop(Tid, LimitRef, Limit, TRef, Interval)
    % after
    %     Interval ->
    %         exit(timer_failed)
    end.

%% Uncomment the above io:format's for more info when running the demo's.
% Send 'send_something' every 1000ms/1sec, rate limited to 1 per s,
% Should see only 10
low_rate_demo() ->
    Pid = tempo:start_link(countme, 1, 1000),% 1 Per second
    SPid = spawn(fun() -> sender_loop(Pid, 0) end),
    {ok, TRef} = timer:send_interval(1000, SPid, send_something),
    erlang:send_after(10010, SPid, done).

% sending 5 per second for 10 seconds
% Should see only 10
fast_rate_demo() ->
    Pid = tempo:start_link(countme, 1, 1000),% 1 Per second
    SPid = spawn(fun() -> sender_loop(Pid, 0) end),
    {ok, TRef} = timer:send_interval(200, SPid, send_something),
    erlang:send_after(10000, SPid, done).

% sending 50 per second for 10 seconds
% Should see only 10
very_fast_rate_demo() ->
    Pid = tempo:start_link(countme, 1, 1000),% 1 Per second
    SPid = spawn(fun() -> sender_loop(Pid, 0) end),
    {ok, TRef} = timer:send_interval(20, SPid, send_something),
    erlang:send_after(10000, SPid, done).


sender_loop(Pid, C) ->
    receive
        send_something ->
            Pid ! {self(), hit},
            NC =
                receive
                    limit ->
                        C;
                    ok ->
                        CC=C+1,
                        io:format("~p ", [CC]),
                        CC
                end,
            sender_loop(Pid, NC);
        done ->
            erlang:exit(Pid, kill),
            io:format("~nDONE~n")
    end.