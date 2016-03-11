-module(chokecherry).
-compile([{parse_transform, lager_transform}]).

-export([start/0, stop/0]).

-export([info/1, info/2, info/3,
    warning/1, warning/2, warning/3,
    error/1, error/2, error/3]).

start() ->
    application:start(chokecherry, permanent).

stop() ->
    application:stop(chokecherry).

info(StringFormat) ->
    info(StringFormat, []).

info(Module, StringFormat) when is_atom(Module) ->
    info(Module, StringFormat, []);
info(StringFormat, Args) ->
    chokecherry_shaper:put(StringFormat, Args, []).

info(Module, StringFormat, Args) ->
    chokecherry_shaper:put(StringFormat, Args, [{module, Module}, {pid, self()}]).

warning(StringFormat) ->
    lager:warning(StringFormat).

warning(Module, StringFormat) when is_atom(Module) ->
    warning(Module, StringFormat, []);
warning(StringFormat, Args) ->
    lager:warning(StringFormat, Args).

warning(Module, StringFormat, Args) ->
    lager:warning(
        io_lib:format("~s:~s ", [atom_to_list(Module), pid_to_list(self())]) ++ StringFormat,
        Args).

error(StringFormat) ->
    lager:error(StringFormat).

error(Module, StringFormat) when is_atom(Module) ->
    error(Module, StringFormat, []);
error(StringFormat, Args) ->
    lager:error(StringFormat, Args).

error(Module, StringFormat, Args) ->
    lager:error(
        io_lib:format("~s:~s ", [atom_to_list(Module), pid_to_list(self())]) ++ StringFormat,
        Args).
