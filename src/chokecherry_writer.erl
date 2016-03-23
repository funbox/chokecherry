-module(chokecherry_writer).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([poll/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("chokecherry.hrl").

-record(state, {
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec poll() -> 'ok'.
poll() ->
    gen_server:cast(?MODULE, poll).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(poll, State) ->
    ok = poll(fun chokecherry_shaper:get/0),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec poll(fun(() -> 'ok' | {term(), term(), term()})) -> 'ok'.
poll(F) ->
    poll(F, F()).

-spec poll(fun(() -> 'ok' | {term(), term(), term()}), 'ok' | {term(), term(), term()}) -> 'ok'.
poll(F, ok) ->
    ok;

poll(F, {StringFormat, Args, Metadata}) ->
    lager:log(info, Metadata, StringFormat, Args),
    poll(F, F()).

