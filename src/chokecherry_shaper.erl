-module(chokecherry_shaper).
-behaviour(gen_server).

-include("chokecherry.hrl").

-define(SERVER, ?MODULE).
-define(WRITER, chokecherry_writer).

-record(state, {
    log_queue :: queue:queue(),
    log_queue_len :: integer(),
    buffer :: {integer(), list(), list()},
    dropped :: integer(),
    last_time :: integer(),
    log_queue_capacity :: integer(),
    timeout :: integer()
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

get(FirstMessage) ->
    gen_server:call(?SERVER, {get, FirstMessage}).

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
    State = #state{
                log_queue = queue:new(),
                log_queue_len = 0,
                buffer = undefined,
                dropped = 0,
                last_time = os:timestamp(),
                log_queue_capacity = log_queue_capacity(),
                timeout = timeout()
            },
    {ok, State}.

handle_call({put, StringFormat, Args, Metadata}, _From, State = #state{log_queue=LogQueue, log_queue_len=LogQueueLen, log_queue_capacity=LogQueueCapacity, timeout=Timeout}) ->
    State2 = case LogQueueLen < LogQueueCapacity of
        true ->
            LogQueue2 = queue:in({StringFormat, Args, Metadata}, LogQueue),
            LogQueueLen2 = LogQueueLen + 1,
            State#state{log_queue=LogQueue2, log_queue_len=LogQueueLen2};
        false ->
            {{value, _}, LogQueue2} = queue:out(LogQueue),
            LogQueue3 = queue:in({StringFormat, Args, Metadata}, LogQueue2),
            State#state{log_queue=LogQueue3, dropped=State#state.dropped+1}
    end,
    State3 = handle_dropped(State2),
    send_new_data(LogQueueLen),
    {reply, ok, State3, Timeout};
handle_call({get, true}, _From, State = #state{log_queue_len=LogQueueLen, buffer=Buffer, timeout=Timeout})
    when Buffer =/= undefined ->
    Reply = {LogQueueLen, Buffer},
    State2 = handle_dropped(State),
    {reply, Reply, State2, Timeout};
handle_call({get, _FirstMessage}, _From, State = #state{log_queue=LogQueue, log_queue_len=LogQueueLen, timeout=Timeout})
    when LogQueueLen > 0 ->
    {{value, Log}, LogQueue2} = queue:out(LogQueue),
    LogQueueLen2 = LogQueueLen - 1,
    State2 = State#state{log_queue=LogQueue2, log_queue_len=LogQueueLen2, buffer=Log},
    State3 = handle_dropped(State2),
    {reply, {LogQueueLen2, Log}, State3, Timeout};
handle_call({get, _FirstMessage}, _From, State = #state{timeout=Timeout}) ->
    State2 = handle_dropped(State),
    {reply, undefined, State2, Timeout};
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

handle_dropped(State = #state{dropped=Dropped, last_time=LastTime}) when Dropped > 0 ->
    {M, S, _} = Now = os:timestamp(),
    case LastTime of
        {M, S, _} ->
            State#state{dropped=Dropped, last_time=Now};
        _ ->
            error_logger:error_report(io_lib:format("chokecherry dropped ~p messages in the last second", [Dropped])),
            State#state{dropped=0, last_time=Now}
    end;
handle_dropped(State) -> State#state{last_time=os:timestamp()}.

send_new_data(0) -> gen_server:cast(?WRITER, new_data);
send_new_data(_) -> nop.

log_queue_capacity() ->
    config(log_queue_capacity, ?SHAPER_LOG_QUEUE_CAPACITY).

timeout() ->
    config(timeout, ?SHAPER_TIMEOUT).

config(Key, Default) ->
    Config = application:get_env(chokecherry, shaper, []),
    proplists:get_value(Key, Config, Default).

