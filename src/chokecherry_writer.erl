-module(chokecherry_writer).
-behaviour(gen_server).

-include("chokecherry.hrl").

-define(SERVER, ?MODULE).
-define(SHAPER, chokecherry_shaper).

-compile([{parse_transform, lager_transform}]).

-record(state, {first_message :: boolean(), timeout :: integer()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    gen_server:cast(self(), loop),
    {ok, #state{
            timeout = config(timeout, ?WRITER_TIMEOUT),
            first_message = true
         }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(loop, State = #state{first_message=FirstMessage, timeout = Timeout}) ->
    case ?SHAPER:get(FirstMessage) of
        undefined ->
            nop;
        {Len, {StringFormat, Args, Metadata}} ->
            lager:log(info, Metadata, StringFormat, Args),
            if Len > 0 -> gen_server:cast(self(), loop);
                true -> ok
            end
    end,
    State2 = State#state{first_message=false},
    {noreply, State2, Timeout};
handle_cast(new_data, State) ->
    flush_new_data(),
    gen_server:cast(self(), loop),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    gen_server:cast(self(), loop),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

flush_new_data() ->
    receive
        {_, new_data} -> flush_new_data()
    after 0 ->
        ok
    end.

config(Key, Default) ->
    Config = application:get_env(chokecherry, writer, []),
    proplists:get_value(Key, Config, Default).
