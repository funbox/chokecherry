-module(shaper_info).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(TIMEOUT, 1000).
-define(LEVEL, info).

-compile([{parse_transform, lager_transform}]).

-record(state, {mps :: integer(), mps_limit :: integer(), last_time :: integer()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([handle/2]).

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

handle(StringFormat, Args) ->
    gen_server:call(?SERVER, {handle, StringFormat, Args}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Config = case application:get_env(chokecherry, ?LEVEL) of
        {ok, C} -> C;
        undefined -> []
    end,
    MpsLimit = proplists:get_value(mps_limit, Config),
    State = #state{mps=1, mps_limit=MpsLimit, last_time=os:timestamp()},
    {ok, State}.

handle_call({handle, StringFormat, Args}, _From, State = #state{mps_limit=undefined}) ->
    lager:?LEVEL(StringFormat, Args),
    {reply, ok, State};
handle_call({handle, StringFormat, Args}, _From, State = #state{mps=Mps, mps_limit=MpsLimit}) when Mps =< MpsLimit ->
    lager:?LEVEL(StringFormat, Args),
    {reply, ok, State#state{mps=Mps+1, last_time=os:timestamp()}, ?TIMEOUT};
handle_call({handle, StringFormat, Args}, _From, State = #state{mps=Mps, mps_limit=MpsLimit, last_time=LastTime}) ->
    {M, S, _} = Now = os:timestamp(),
    Overlimit = Mps - MpsLimit, 
    Drop = Overlimit - 1,
    NewState = case LastTime of
        {M, S, _} -> 
            if Overlimit < 1 -> lager:?LEVEL(StringFormat, Args);
                true -> ok
            end,
            State#state{mps=Mps+1};
        _ -> 
            case Overlimit > 0 of
                true -> lager:?LEVEL("chokecherry dropped ~p messages in the last second that exceeded the limit of ~p messages/sec", 
                [Drop, MpsLimit]);
                false -> lager:?LEVEL(StringFormat, Args)
            end,
            State#state{mps=1, last_time=Now}
    end,
    {reply, ok, NewState, ?TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State = #state{mps=Mps, mps_limit=MpsLimit}) ->
    Overlimit = Mps - MpsLimit, 
    Drop = Overlimit - 1,
    if Overlimit > 0 -> lager:?LEVEL("chokecherry dropped ~p messages in the last second that exceeded the limit of ~p messages/sec", 
        [Drop, MpsLimit]);
        true -> ok
    end,
    NewState = State#state{mps=1, last_time=os:timestamp()},
    {noreply, NewState, ?TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{mps=Mps, mps_limit=MpsLimit}) ->
    Overlimit = Mps - MpsLimit, 
    Drop = Overlimit - 1,
    if Overlimit > 0 -> lager:?LEVEL("chokecherry dropped ~p messages in the last second that exceeded the limit of ~p messages/sec", 
        [Drop, MpsLimit]);
        true -> ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

