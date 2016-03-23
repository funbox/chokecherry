-module(chokecherry_shaper_logger).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([shaped_report/2,
         add_shaped_report_handler/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_event:start_link({local, ?MODULE}).

-spec shaped_report(string(), non_neg_integer()) -> 'ok'.
shaped_report(Msg, Dropped) ->
    ok = error_logger:error_msg(Msg, [Dropped]),
    ok = gen_event:notify(?MODULE, {shaped_report, Msg, Dropped}).

-spec add_shaped_report_handler(atom()) -> 'ok' | term().
add_shaped_report_handler(Handler) ->
    gen_event:add_handler(?MODULE, Handler, []).

