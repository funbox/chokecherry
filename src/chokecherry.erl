-module(chokecherry).

-export([start/0, stop/0]).

-export([info/2, warning/2]).

start() ->
    application:start(chokecherry, permanent).

stop() ->
    application:stop(chokecherry).

info(StringFormat, Args) ->
    shaper_info:handle(StringFormat, Args).

warning(StringFormat, Args) ->
    shaper_warning:handle(StringFormat, Args).
