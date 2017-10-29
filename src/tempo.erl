-module(tempo).

-export([
    low_rate_demo/0,
    fast_rate_demo/0,
    start_link/2,
    init/2
]).

start_link(LimitRef, LimitPSec) ->
    proc_lib:start_link(?MODULE, init, [LimitRef, LimitPSec]).

init(LimitRef, LimitPSec) ->
    Tid = ets:new(LimitRef, [set, private]),
    true = ets:insert(Tid, {LimitRef, 0}),
    {ok, TRef} = timer:send_interval(1000, cleanup),
    loop_init(Tid, LimitRef, LimitPSec, TRef).

loop_init(Tid, LimitRef, LimitPSec, TRef) ->
    proc_lib:init_ack(self()),
    loop(Tid, LimitRef, LimitPSec, TRef).

loop(Tid, LimitRef, LimitPSec, TRef) ->
    receive
        {ReqPid, hit} ->
            case ets:update_counter(Tid, LimitRef, 1) of
                NewCount when NewCount >= LimitPSec+1 ->
                    % io:format("LC ~p~n", [NewCount]),
                    % io:format("L ~p~n", [erlang:monotonic_time(nano_seconds)]),
                    ReqPid ! limit;
                _NewCount ->
                    % io:format("HC ~p~n", [NewCount]),
                    % io:format("H ~p~n", [erlang:monotonic_time(nano_seconds)]),
                    ReqPid ! ok
            end,
            loop(Tid, LimitRef, LimitPSec, TRef);
        cleanup ->
            % io:format("C~p~n", [erlang:monotonic_time(nano_seconds)]),
            true = ets:insert(Tid, {LimitRef, 0}),
            loop(Tid, LimitRef, LimitPSec, TRef)
    after
        1005 ->
            exit(timer_failed)
    end.

%% Just uncomment the above io:format's for more info when running the demo's.
low_rate_demo() ->
    Pid = tempo:start_link(countme, 1),
    timer:sleep(10),
    [ begin
        timer:sleep(1001),
        Pid ! {self(), hit}
      end || _X <- lists:seq(1, 5)
    ],
    erlang:exit(Pid, kill).

fast_rate_demo() ->
    Pid = tempo:start_link(countme, 1),
    timer:sleep(10),
    [ begin
        timer:sleep(101),
        Pid ! {self(), hit}
      end || _X <- lists:seq(1, 100)
    ],
    erlang:exit(Pid, kill).