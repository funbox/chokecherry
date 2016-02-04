-module(chokecherry_shaper).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(TIMEOUT, 1000).
-define(WRITER, chokecherry_writer).

-define(LOG_QUEUE_CAPACITY, 10000).
-define(SYS_QUEUE_CAPACITY, 10000).

-record(state, {sys_queue :: queue:queue(),
    sys_queue_len :: integer(),
    log_queue :: queue:queue(),
    log_queue_len :: integer(),
    buffer :: {integer(), list(), list()},
    log_id :: integer(),
    max_log_id :: integer(),
    dropped :: integer(),
    last_time :: integer()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([get/1, put/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(LogId) ->
    gen_server:call(?SERVER, {get, LogId}).

put(StringFormat, Args, Metadata) ->
    try
        gen_server:call(?SERVER, {put, StringFormat, Args, Metadata})
    catch
        exit:{noproc, _} -> {error, chokecherry_not_started}
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    State = #state{sys_queue=queue:new(), sys_queue_len=0, log_queue=queue:new(), log_queue_len=0, buffer=undefined,
        log_id=0, max_log_id=?LOG_QUEUE_CAPACITY+?SYS_QUEUE_CAPACITY-1, dropped=0, last_time=os:timestamp()},
    {ok, State}.

handle_call({put, StringFormat, Args, Metadata}, _From, State = #state{log_queue=LogQueue, log_queue_len=LogQueueLen,
    log_id=LogId}) when LogQueueLen < ?LOG_QUEUE_CAPACITY ->
    LogQueue2 = queue:in({LogId, StringFormat, Args, Metadata}, LogQueue),
    LogQueueLen2 = LogQueueLen + 1,
    LogId2 = get_next_log_id(State),
    State2 = State#state{log_queue=LogQueue2, log_queue_len=LogQueueLen2, log_id=LogId2},
    State3 = handle_dropped(State2),
    send_new_data(State3),
    {reply, ok, State3, ?TIMEOUT};
handle_call({put, StringFormat, Args, Metadata}, _From, State = #state{log_queue=LogQueue, log_id=LogId, dropped=Dropped}) ->
    {{value, _}, LogQueue2} = queue:out(LogQueue),
    LogQueue3 = queue:in({LogId, StringFormat, Args, Metadata}, LogQueue2),
    LogId2 = get_next_log_id(State),
    State2 = State#state{log_queue=LogQueue3, dropped=Dropped+1, log_id=LogId2},
    State3 = handle_dropped(State2),
    send_new_data(State3),
    {reply, ok, State3, ?TIMEOUT};
handle_call({get, _LastLogId}, _From, State = #state{sys_queue=SysQueue, sys_queue_len=SysQueueLen,
    log_queue_len=LogQueueLen, buffer=Buffer}) when SysQueueLen > 0 andalso Buffer =:= undefined ->
    {{value, Log}, SysQueue2} = queue:out(SysQueue),
    SysQueueLen2 = SysQueueLen - 1,
    Reply = {LogQueueLen + SysQueueLen2, Log},
    State2 = State#state{sys_queue=SysQueue2, sys_queue_len=SysQueueLen2, buffer=Log},
    State3 = handle_dropped(State2),
    {reply, Reply, State3, ?TIMEOUT};
handle_call({get, LastLogId}, _From, State = #state{sys_queue=SysQueue, sys_queue_len=SysQueueLen,
    log_queue_len=LogQueueLen, buffer=Buffer}) when SysQueueLen > 0 ->
    {BufferLogId, _, _} = Buffer,
    {Reply, State2} = case LastLogId =:= BufferLogId of
        true ->
            {{value, Log}, SysQueue2} = queue:out(SysQueue),
            SysQueueLen2 = SysQueueLen - 1,
            {{LogQueueLen + SysQueueLen2, Log}, State#state{sys_queue=SysQueue2, sys_queue_len=SysQueueLen2, buffer=Log}};
        false ->
            {{LogQueueLen + SysQueueLen, Buffer}, State}
    end,
    State3 = handle_dropped(State2),
    {reply, Reply, State3, ?TIMEOUT};
handle_call({get, _LastLogId}, _From, State = #state{log_queue=LogQueue, log_queue_len=LogQueueLen, buffer=Buffer})
    when LogQueueLen > 0 andalso Buffer == undefined ->
    {{value, Log}, LogQueue2} = queue:out(LogQueue),
    LogQueueLen2 = LogQueueLen - 1,
    State2 = State#state{log_queue=LogQueue2, log_queue_len=LogQueueLen2, buffer=Log},
    State3 = handle_dropped(State2),
    {reply, {LogQueueLen2, Log}, State3, ?TIMEOUT};
handle_call({get, LastLogId}, _From, State = #state{log_queue=LogQueue, log_queue_len=LogQueueLen, buffer=Buffer})
    when LogQueueLen > 0 ->
    {BufferLogId, _, _, _} = Buffer,
    {Reply, State2} = case LastLogId =:= BufferLogId of
        true ->
            {{value, Log}, LogQueue2} = queue:out(LogQueue),
            LogQueueLen2 = LogQueueLen - 1,
            {{LogQueueLen2, Log}, State#state{log_queue=LogQueue2, log_queue_len=LogQueueLen2, buffer=Log}};
        false ->
            {{LogQueueLen, Buffer}, State}
    end,
    State3 = handle_dropped(State2),
    {reply, Reply, State3, ?TIMEOUT};
handle_call({get, _LastLogId}, _From, State) ->
    State2 = handle_dropped(State),
    {reply, undefined, State2, ?TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    State2 = handle_dropped(State),
    {noreply, State2};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_next_log_id(#state{log_id=LogId, max_log_id=MaxLogId}) when LogId < MaxLogId -> LogId + 1;
get_next_log_id(_State) -> 0.

send_new_data(#state{log_id=LogId}) when LogId rem 100 =:= 0 -> gen_server:cast(?WRITER, new_data);
send_new_data(_State) -> ok.

handle_dropped(State = #state{sys_queue=SysQueue, sys_queue_len=SysQueueLen, log_id=LogId, dropped=Dropped,
    last_time=LastTime}) when Dropped > 0 ->
    {M, S, _} = Now = os:timestamp(),
    case LastTime of
        {M, S, _} ->
            State#state{dropped=Dropped, last_time=Now};
        _ ->
            SysStringFormat = "chokecherry dropped ~p messages in the last second",
            {SysQueue3, SysQueueLen3} = case SysQueueLen < ?SYS_QUEUE_CAPACITY of
                true ->
                    {queue:in({LogId, SysStringFormat, [Dropped]}, SysQueue), SysQueueLen + 1};
                false ->
                    {{value, {_Id, _FormatString, [D]}}, SysQueue2} = queue:out(SysQueue),
                    {queue:in({LogId, SysStringFormat, [Dropped + D]}, SysQueue2), SysQueueLen}
            end,
            LogId2 = get_next_log_id(State),
            State#state{sys_queue=SysQueue3, sys_queue_len=SysQueueLen3, log_id=LogId2, dropped=0, last_time=Now}
    end;
handle_dropped(State) -> State#state{last_time=os:timestamp()}.
