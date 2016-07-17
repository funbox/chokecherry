-module(chokecherry).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0,
         stop/0]).

-export([info/1,
         info/2,
         info/3,
         info/4, 
         warning/1,
         warning/2,
         warning/3,
         error/1,
         error/2,
         error/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-spec start() -> 'ok'.
start() ->
    {ok, _} = application:ensure_all_started(chokecherry),
    ok.

-spec stop() -> 'ok'.
stop() ->
    ok = application:stop(chokecherry).


-spec info(string()) -> 'ok'.
info(StringFormat) ->
    info(StringFormat, []).

-spec info(atom() | string(), string() | list()) -> 'ok'.
info(Module, StringFormat) when is_atom(Module) ->
    info(Module, StringFormat, []);

info(StringFormat, Args) ->
    chokecherry_shaper:put({[], StringFormat, Args}).

-spec info(atom(), string(), list()) -> 'ok'.
info(Module, StringFormat, Args) ->
    chokecherry_shaper:put({[{module, Module}, {pid, self()}], StringFormat, Args}).

-spec info(atom(), list(), string(), list()) -> 'ok'.
info(Module, [MetaData], StringFormat, Args) ->
    chokecherry_shaper:put({[{module, Module}, {pid, self()}, MetaData], StringFormat, Args}).


-spec warning(string()) -> 'ok'.
warning(StringFormat) ->
    lager:warning(StringFormat).

-spec warning(atom() | string(), string() | list()) -> 'ok'.
warning(Module, StringFormat) when is_atom(Module) ->
    warning(Module, StringFormat, []);

warning(StringFormat, Args) ->
    lager:warning(StringFormat, Args).

-spec warning(atom(), string(), list()) -> 'ok'.
warning(Module, StringFormat, Args) ->
    lager:log(warning, [{module, Module}, {pid, self()}], StringFormat, Args).


-spec error(string()) -> 'ok'.
error(StringFormat) ->
    lager:error(StringFormat).

-spec error(atom() | string(), string() | list()) -> 'ok'.
error(Module, StringFormat) when is_atom(Module) ->
    error(Module, StringFormat, []);

error(StringFormat, Args) ->
    lager:error(StringFormat, Args).

-spec error(atom(), string(), list()) -> 'ok'.
error(Module, StringFormat, Args) ->
    lager:log(error, [{module, Module}, {pid, self()}], StringFormat, Args).
