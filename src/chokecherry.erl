-module(chokecherry).

-export([start/0, stop/0]).

-export([info/1, info/2]).

start() ->
    application:start(chokecherry, permanent).

stop() ->
    application:stop(chokecherry).

info(StringFormat) ->
    info(StringFormat, []).

info(StringFormat, Args) ->
    chokecherry_shaper:put(StringFormat, Args).
