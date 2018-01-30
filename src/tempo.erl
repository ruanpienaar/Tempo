-module(tempo).

% TODO: Use a compile macro, to add debug logging.
% -ifdef(DEBUG).
%    io:format("...")
% -endif

-export([
    start_link/3,
    init/3,
    worker_init/0
]).

-export([
    low_limit_low_rate_demo/0,
    low_limit_high_rate_demo/0,
    low_limit_very_high_rate_demo/0,
    high_limit_low_rate_demo/0,
    high_limit_high_rate_demo/0    
]).

start_link(LimitRef, Limit, Interval) ->
    proc_lib:start_link(?MODULE, init, [LimitRef, Limit, Interval]).

init(LimitRef, Limit, Interval) ->
    % can later spawn multiple workers if needed.
    WorkerLoopPid = proc_lib:start_link(?MODULE, worker_init, []),
    Tid = ets:new(LimitRef, [set, private]),
    true = ets:insert(Tid, {LimitRef, 0}),
    {ok, TRef} = timer:send_interval(Interval, cleanup),
    % we could pass a list of worker pids, and spread the load.
    loop_init(Tid, LimitRef, Limit, TRef, Interval, WorkerLoopPid).

worker_init() ->
    Tid = ets:new(storage, [set, {write_concurrency, true}]),
    ok = proc_lib:init_ack(self()),
    worker_loop(Tid).

loop_init(Tid, LimitRef, Limit, TRef, Interval, WPid) ->
    ok = proc_lib:init_ack(self()),
    loop(Tid, LimitRef, Limit, TRef, Interval, WPid).

loop(Tid, LimitRef, Limit, TRef, Interval, WPid) ->
    receive
        cleanup ->
            WPid ! {self(), cleanup},
            receive
                {WPid, ok} ->
                    ok
            after 
                1000 ->
                    exit({worker_cleanup_timeout})
            end,
            true = ets:insert(Tid, {LimitRef, 0}),
            loop(Tid, LimitRef, Limit, TRef, Interval, WPid);
        {ReqPid, hit} ->
            case ets:update_counter(Tid, LimitRef, 1) of
                NewCount when NewCount >= Limit+1 -> 
                %( +1 needed cause update_counter adds 1 )
                    ReqPid ! limit;
                _ ->
                    WPid ! hit,
                    ReqPid ! ok
            end,
            loop(Tid, LimitRef, Limit, TRef, Interval, WPid)
    % after
    %     Interval ->
    %         exit(timer_failed)
    end.

worker_loop(Tid) ->
    receive
        hit ->
            true = ets:insert(Tid, {erlang:monotonic_time(nano_seconds), 0}),
            worker_loop(Tid);
        {ReqPid, cleanup} ->
            io:format("worker size ~p~n", [ets:info(Tid, size)]),
            true = ets:delete_all_objects(Tid),
            ReqPid ! {self(), ok},
            worker_loop(Tid);
        R ->
            exit({unknown_message, R})
    end.

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