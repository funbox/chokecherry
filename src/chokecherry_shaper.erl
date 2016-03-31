-module(chokecherry_shaper).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([get/0,
         put/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    log_queue               :: queue:queue(),
    log_queue_len           :: non_neg_integer(),
    log_queue_capacity      :: non_neg_integer(),

    dropped                 :: non_neg_integer(),
    last_time               :: non_neg_integer(),

    timeout                 :: non_neg_integer()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec get() -> 'ok' | {term(), term(), term()}.
get() ->
    gen_server:call(?MODULE, get).

-spec put({list(), string(), list()}) -> 'ok' | {'error', 'chokecherry_not_started'}.
put(Log) ->
    try
        gen_server:call(?MODULE, {put, Log})
    catch
        exit:{noproc, _} -> {error, chokecherry_not_started}
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    State = #state{
        log_queue           = queue:new(),
        log_queue_len       = 0,
        log_queue_capacity  = chokecherry_config:log_queue_capacity(),

        dropped             = 0,
        last_time           = os:timestamp(),

        timeout             = chokecherry_config:shaper_timeout()
    },
    {ok, State}.


handle_call({put, Log}, From, #state{log_queue = Queue, log_queue_len = 0, timeout = Timeout} = State) ->
    gen_server:reply(From, ok),
    ok = chokecherry_writer:poll(),
    {noreply, handle_dropped(State#state{log_queue = queue:in(Log, Queue), log_queue_len = 1}), Timeout};

handle_call({put, Log}, From, #state{log_queue = Queue, log_queue_len = QueueLen, log_queue_capacity = QueueCapacity, timeout = Timeout} = State) when QueueLen < QueueCapacity ->
    gen_server:reply(From, ok),
    {noreply, handle_dropped(State#state{log_queue = queue:in(Log, Queue), log_queue_len = QueueLen + 1}), Timeout};

handle_call({put, Log}, From, #state{log_queue = Queue, dropped = Dropped, timeout = Timeout} = State) ->
    gen_server:reply(From, ok),
    {_, NewQueue} = queue:out(Queue),
    {noreply, handle_dropped(State#state{log_queue = queue:in(Log, NewQueue), dropped = Dropped + 1}), Timeout};


handle_call(get, From, #state{log_queue_len = 0, timeout = Timeout} = State) ->
    gen_server:reply(From, ok),
    {noreply, handle_dropped(State), Timeout};

handle_call(get, From, #state{log_queue = Queue, log_queue_len = QueueLen, timeout = Timeout} = State) ->
    {{value, Log}, NewQueue} = queue:out(Queue),
    gen_server:reply(From, Log),
    {noreply, handle_dropped(State#state{log_queue = NewQueue, log_queue_len = QueueLen - 1}), Timeout};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, handle_dropped(State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_dropped(#state{dropped = Dropped, last_time = LastTime} = State) when Dropped > 0 ->
    {M, S, _} = Now = os:timestamp(),
    case LastTime of
        {M, S, _} ->
            State#state{dropped = Dropped, last_time = Now};
        _ ->
            ok = chokecherry_shaper_logger:shaped_report("chokecherry dropped ~w messages in the last second", Dropped),
            State#state{dropped = 0, last_time = Now}
    end;

handle_dropped(State) ->
    State#state{last_time = os:timestamp()}.

