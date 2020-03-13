-module(tempo).

% TODO: Use a compile macro, to add debug logging.
% -ifdef(DEBUG).
%    io:format("...")
% -endif

% API
-export([
    start_link/3,
    can_make_call/1,
    can_make_call/2
]).

-export([
    init/3,
    worker_init/1
]).

%% ------------------------------------------------------------

%% @doc Interval is in miliseconds.
%% @end
-spec start_link(atom, pos_integer(), pos_integer()) -> {ok, pid()}.
start_link(LimitTbl, Limit, Interval) ->
    {ok, proc_lib:start_link(?MODULE, init, [LimitTbl, Limit, Interval])}.

-spec can_make_call(pid()) -> boolean().
can_make_call(Pid) ->
    can_make_call(Pid, 5000).

-spec can_make_call(pid(), non_neg_integer()) -> boolean().
can_make_call(Pid, Timeout) ->
    Pid ! {self(), hit},
    receive
        limit ->
            false;
        ok ->
            true
    after
    	Timeout ->
    	    timeout
    end.

%% ------------------------------------------------------------

init(LimitTbl, Limit, Interval) ->
    WorkerLoopPid = proc_lib:start_link(?MODULE, worker_init, [LimitTbl]),
    true = erlang:register(LimitTbl, self()),
    rest_of_init(LimitTbl, Limit, Interval, WorkerLoopPid).

rest_of_init(LimitTbl, Limit, Interval, WorkerLoopPid) ->
    Tid = ets:new(LimitTbl, [set, private]),
    true = ets:insert(Tid, {LimitTbl, 0}),
    {ok, TRef} = timer:send_interval(Interval, cleanup),
    % we could pass a list of worker pids, and spread the load.
    loop_init(Tid, LimitTbl, Limit, TRef, Interval, WorkerLoopPid).

worker_init(Name) ->
    Tid = ets:new(Name, [set, {write_concurrency, true}]),
    ok = proc_lib:init_ack(self()),
    worker_loop(Tid).

loop_init(Tid, LimitTbl, Limit, TRef, Interval, WPid) ->
    ok = proc_lib:init_ack(self()),
    loop(Tid, LimitTbl, Limit, TRef, Interval, WPid).

loop(Tid, LimitTbl, Limit, TRef, Interval, WPid) ->
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
            true = ets:insert(Tid, {LimitTbl, 0}),
            loop(Tid, LimitTbl, Limit, TRef, Interval, WPid);
        {ReqPid, hit} ->
            case ets:update_counter(Tid, LimitTbl, 1) of
                NewCount when NewCount >= Limit+1 ->
                %( +1 is needed cause update_counter adds 1 )
                    ReqPid ! limit;
                _ ->
                    WPid ! hit,
                    ReqPid ! ok
            end,
            loop(Tid, LimitTbl, Limit, TRef, Interval, WPid)
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
            % io:format("worker size ~p~n", [ets:info(Tid, size)]),
            true = ets:delete_all_objects(Tid),
            ReqPid ! {self(), ok},
            worker_loop(Tid);
        R ->
            exit({unknown_message, R})
    end.
