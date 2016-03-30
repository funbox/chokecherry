-module(chokecherry_config).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([log_queue_capacity/0,
         shaper_timeout/0]).

-include("chokecherry.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec log_queue_capacity() -> non_neg_integer().
log_queue_capacity() ->
    ShaperConfig = application:get_env(chokecherry, shaper, []),
    proplists:get_value(log_queue_capacity, ShaperConfig, ?SHAPER_LOG_QUEUE_CAPACITY).

-spec shaper_timeout() -> non_neg_integer().
shaper_timeout() ->
    ShaperConfig = application:get_env(chokecherry, shaper, []),
    proplists:get_value(shaper_timeout, ShaperConfig, ?SHAPER_TIMEOUT).

