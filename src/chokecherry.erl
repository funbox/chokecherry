-module(chokecherry).
-compile([{parse_transform, chokecherry_transform}]).

-export([start/0, stop/0]).

-export([info/1, info/2, info/3]).

start() ->
    application:start(chokecherry, permanent).

stop() ->
    application:stop(chokecherry).

info(StringFormat) ->
    info(StringFormat, []).

info(Module, StringFormat) when is_atom(Module) ->
    info(Module, StringFormat, []);
info(StringFormat, Args) ->
    chokecherry_shaper:put(StringFormat, Args).

info(Module, StringFormat, Args) ->
    chokecherry_shaper:put(
        io_lib:format("<< ~s:~s ", [atom_to_list(Module), pid_to_list(self())]) ++ StringFormat,
        Args).
